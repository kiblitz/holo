open! Core
open Import

module State = struct
  module Stack_component = struct
    type t =
      | Expr of Ast.Expr.t
      | Of_expr of
          { priority : Priority.t
          ; f : Ast.Expr.t -> Ast.Expr.t
          }
      | Comma
    [@@deriving sexp_of]

    let constructor id =
      let f payload = Ast.Expr.Variant { tag = id; payload = Some payload } in
      Of_expr { priority = Priority.constructor; f }
    ;;

    let func_apply caller =
      let f arg = Ast.Expr.Call { caller; arg } in
      Of_expr { priority = Priority.func_apply; f }
    ;;

    let scope binding to_ =
      let f in_ = Ast.Expr.Scope { binding; to_; in_ } in
      Of_expr { priority = Priority.scope; f }
    ;;

    let prefix symbol =
      let f expr = Ast.Expr.Prefix { symbol; t = expr } in
      Of_expr { priority = Priority.prefix symbol; f }
    ;;

    let infix expr symbol =
      let f r_expr = Ast.Expr.Infix { left_t = expr; symbol; right_t = r_expr } in
      Of_expr { priority = Priority.infix symbol; f }
    ;;
  end

  module Context = struct
    type t =
      | Toplevel
      | Scope
      | Parenthesis
      | Curly_bracket
      | Square_bracket
    [@@deriving equal, sexp_of]
  end

  type t =
    { stack : Stack_component.t list
    ; unconsumed_tokens : Token.t Source_position.With_section.t list
    }

  let init tokens = { stack = []; unconsumed_tokens = tokens }
end

module Result = struct
  type t =
    { expr : Ast.Expr.t
    ; unconsumed_tokens : Token.t Source_position.With_section.t list
    }
end

let reduce_stack ?next_priority stack =
  match (stack : State.Stack_component.t list) with
  | Expr expr :: Of_expr { priority; f } :: rest_of_stack ->
    let stack_with_reduced =
      let reduced = State.Stack_component.Expr (f expr) in
      reduced :: rest_of_stack
    in
    Ok
      (match next_priority with
       | None -> Some stack_with_reduced
       | Some next_priority ->
         if
           Priority.(
             Comparison_result.equal (compare ~left:priority ~right:next_priority) Reduce)
         then Some stack_with_reduced
         else None)
  | [ Expr _ ] | Expr _ :: Comma :: _ | [] -> Ok None
  | Of_expr _ :: _ ->
    Or_error.error_s
      [%message "Cannot reduce an of_expression" (stack : State.Stack_component.t list)]
  | Comma :: _ ->
    Or_error.error_s
      [%message "Cannot reduce a comma" (stack : State.Stack_component.t list)]
  | Expr _ :: _ ->
    Or_error.error_s
      [%message
        "Invariant broken: Consecutive expressions" (stack : State.Stack_component.t list)]
;;

let reduce_stack_all stack ~(context : State.Context.t) =
  let rec loop ?comma_list = function
    | State.Stack_component.Expr expr :: State.Stack_component.Comma :: rest_of_stack ->
      let comma_list =
        match comma_list with
        | None -> [ expr ]
        | Some list -> expr :: list
      in
      loop ~comma_list rest_of_stack
    | [ State.Stack_component.Expr expr ] ->
      Ok
        (match comma_list, context with
         | None, Square_bracket -> Ast.Expr.List { children = [ expr ] }
         | None, (_ : State.Context.t) -> expr
         | Some comma_list, Square_bracket ->
           Ast.Expr.List { children = expr :: comma_list }
         | Some comma_list, (_ : State.Context.t) ->
           Ast.Expr.Tuple { children = expr :: comma_list })
    | current_stack ->
      (match%bind.Or_error reduce_stack current_stack with
       | None ->
         Or_error.error_s
           [%message "Failed expression reduction" (stack : State.Stack_component.t list)]
       | Some stack -> loop ?comma_list stack)
  in
  loop stack
;;

(* If the top of the parse stack is an expression, then that expression is actually the caller of
     a function application.

     In this scenario, prior to pushing the function application, we reduce the parse stack as long
     as the top of stack has higher operator precedence (we maintain the invariant that the parse
     stack's element priorities are monotonically increasing).
*)
let rec push_expr_onto_stack stack expr =
  match (stack : State.Stack_component.t list) with
  | Expr caller_or_constructor :: rest_of_stack ->
    let priority, of_expr =
      match caller_or_constructor with
      | Variant { tag; payload = None } ->
        Priority.constructor, State.Stack_component.constructor tag
      | caller -> Priority.func_apply, State.Stack_component.func_apply caller
    in
    (match%bind.Or_error reduce_stack ~next_priority:priority stack with
     | None -> Ok (expr :: of_expr :: rest_of_stack)
     | Some reduced_stack -> push_expr_onto_stack reduced_stack expr)
  | stack -> Ok (expr :: stack)
;;

(* If the top of the parse stack is not an expression, the pushed symbol must necessarily be a
     prefix operator.

     Otherwise, the symbol is an infix operator.

     In either scenario, prior to pushing the symbol we reduce the parse stack as long as the top
     of stack has higher operator precedence (we maintain the invariant that the parse stack's
     element priorities are monotonically increasing).
*)
let rec push_symbol_onto_stack stack symbol =
  match (stack : State.Stack_component.t list) with
  | Of_expr _ :: _ | Comma :: _ | [] ->
    (match symbol with
     | Token.Symbol.Operator.Base symbol ->
       let prefix = State.Stack_component.prefix symbol in
       Ok (prefix :: stack)
     | Non_custom symbol ->
       Or_error.error_s
         [%message
           "Cannot construct prefix from non-custom symbol"
             (symbol : Token.Symbol.Operator.Non_custom.t)])
  | Expr expr :: rest_of_stack ->
    let priority = Priority.infix symbol in
    (match%bind.Or_error reduce_stack ~next_priority:priority stack with
     | None ->
       let infix = State.Stack_component.infix expr symbol in
       Ok (infix :: rest_of_stack)
     | Some reduced_stack -> push_symbol_onto_stack reduced_stack symbol)
;;

let exit_inner_parse
      ~stack
      ~unconsumed_tokens
      ~(context : State.Context.t)
      ~(expected_context : State.Context.t)
  =
  if not ([%equal: State.Context.t] context expected_context)
  then
    Or_error.error_s
      [%message
        "Tried escaping non-existent context"
          (context : State.Context.t)
          (expected_context : State.Context.t)]
  else Ok { State.stack; unconsumed_tokens }
;;

(* Process the resulting state from a "next level" parse (i.e. within parens).

     The result is then appended to the "current level" parse stack.
*)
let parse_inner_state
      ~stack
      ~unconsumed_tokens
      ~(context : State.Context.t)
      ~parse_function
  =
  let%bind.Or_error (inner_state : State.t) =
    parse_function (State.init unconsumed_tokens) ~context
  in
  let%map.Or_error new_stack =
    let%bind.Or_error inner_expr = reduce_stack_all inner_state.stack ~context in
    State.Stack_component.Expr inner_expr |> push_expr_onto_stack stack
  in
  { inner_state with stack = new_stack }
;;

let push_simple_expr_and_parse_rest
      (expr : Ast.Expr.t)
      ~stack
      ~unconsumed_tokens
      ~context
      ~parse_function
  =
  let%bind.Or_error new_stack =
    State.Stack_component.Expr expr |> push_expr_onto_stack stack
  in
  parse_function { State.stack = new_stack; unconsumed_tokens } ~context
;;

let parse_raw tokens =
  let rec loop { State.stack; unconsumed_tokens } ~(context : State.Context.t) =
    match unconsumed_tokens with
    | token :: rest_of_tokens ->
      let unconsumed_tokens = rest_of_tokens in
      (match (Source_position.With_section.value token : Token.t) with
       | Definition Underscore ->
         push_simple_expr_and_parse_rest
           Underscore
           ~stack
           ~unconsumed_tokens
           ~context
           ~parse_function:loop
       | Constant constant ->
         push_simple_expr_and_parse_rest
           (Constant (Constant constant))
           ~stack
           ~unconsumed_tokens
           ~context
           ~parse_function:loop
       | Identifier id ->
         push_simple_expr_and_parse_rest
           (Id id)
           ~stack
           ~unconsumed_tokens
           ~context
           ~parse_function:loop
       | Big_identifier id ->
         push_simple_expr_and_parse_rest
           (Ast.Expr.Variant { tag = id; payload = None })
           ~stack
           ~unconsumed_tokens
           ~context
           ~parse_function:loop
       | Symbol (Operator (Base [ Equal ])) ->
         let%bind.Or_error binding =
           let%bind.Or_error expr = reduce_stack_all stack ~context in
           Util.binding_of_expr expr
         in
         let%bind.Or_error { State.stack; unconsumed_tokens } =
           loop (State.init unconsumed_tokens) ~context:Scope
         in
         let%bind.Or_error new_stack =
           let%map.Or_error to_expr = reduce_stack_all stack ~context in
           [ State.Stack_component.scope binding to_expr ]
         in
         loop { State.stack = new_stack; unconsumed_tokens } ~context
       | Symbol (Operator symbol) ->
         let%bind.Or_error new_stack = push_symbol_onto_stack stack symbol in
         loop { State.stack = new_stack; unconsumed_tokens } ~context
       | Grouping (Parenthesis Left) ->
         let%bind.Or_error state =
           parse_inner_state
             ~stack
             ~unconsumed_tokens
             ~context:Parenthesis
             ~parse_function:loop
         in
         loop state ~context
       | Grouping (Curly_bracket Left) ->
         let%bind.Or_error state =
           parse_inner_state
             ~stack
             ~unconsumed_tokens
             ~context:Curly_bracket
             ~parse_function:loop
         in
         loop state ~context
       | Grouping (Square_bracket Left) ->
         let%bind.Or_error state =
           parse_inner_state
             ~stack
             ~unconsumed_tokens
             ~context:Square_bracket
             ~parse_function:loop
         in
         loop state ~context
       | Grouping (Parenthesis Right) ->
         if List.is_empty stack
         then Ok { State.stack = [ Expr (Constant Unit) ]; unconsumed_tokens }
         else
           exit_inner_parse
             ~stack
             ~unconsumed_tokens
             ~context
             ~expected_context:Parenthesis
       | Grouping (Curly_bracket Right) ->
         exit_inner_parse
           ~stack
           ~unconsumed_tokens
           ~context
           ~expected_context:Curly_bracket
       | Grouping (Square_bracket Right) ->
         exit_inner_parse
           ~stack
           ~unconsumed_tokens
           ~context
           ~expected_context:Square_bracket
       | Definition In ->
         exit_inner_parse ~stack ~unconsumed_tokens ~context ~expected_context:Scope
       | Symbol Comma -> loop { State.stack = Comma :: stack; unconsumed_tokens } ~context
       | Symbol Semicolon -> Ok { State.stack; unconsumed_tokens }
       | Lambda
       | Functor
       | With
       | Symbol _
       | Conditional _
       | Definition _
       | Import _
       | Typeful _ -> Or_error.error_s [%message "Unimplemented"])
    | [] -> Ok { State.stack; unconsumed_tokens }
  in
  let%bind.Or_error { State.stack; unconsumed_tokens } =
    loop (State.init tokens) ~context:Toplevel
  in
  let%map.Or_error expr = reduce_stack_all stack ~context:Toplevel in
  { Result.expr; unconsumed_tokens }
;;

let parse tokens =
  let%bind.Or_error { Result.expr; unconsumed_tokens } = parse_raw tokens in
  let%map.Or_error () = Util.validate_unconsumed_tokens_is_empty unconsumed_tokens in
  expr
;;
