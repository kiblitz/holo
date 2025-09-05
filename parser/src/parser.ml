open! Core
open Import

module Util = struct
  let prefix_priority symbol =
    match (symbol : Token.Symbol.Operator.Base.t Nonempty_list.t) with
    | [ Minus ] | [ Minus; Dot ] -> Priority.neg
    | _ -> Priority.prefix
  ;;

  let infix_priority symbol =
    match (symbol : Token.Symbol.Operator.t) with
    | Base (Times :: Times :: _) -> Priority.pow
    | Base (Times :: _) -> Priority.times
    | Base (At :: _) -> Priority.at
    | Base (Div :: _) -> Priority.divide
    | Base (Percent :: _) -> Priority.percent
    | Base (Caret :: _) -> Priority.caret
    | Base (Plus :: _) -> Priority.plus
    | Base (Minus :: _) | Base (Tilda :: _) -> Priority.minus
    | Base [ Ampersand ] | Base [ Ampersand; Ampersand ] -> Priority.and_
    | Base [ Pipe; Pipe ] -> Priority.or_
    | Base (Less :: _)
    | Base (Equal :: _)
    | Base (Greater :: _)
    | Base (Pipe :: _)
    | Base (Dollar :: _)
    | Base (Ampersand :: _) -> Priority.cmp
    | Base (Dot :: _) | Non_custom Double_colon -> Priority.submodule
  ;;
end

module State = struct
  module Stack_component = struct
    type t =
      | Expr of Ast.Expr.t
      | Of_expr of
          { priority : Priority.t
          ; f : Ast.Expr.t -> Ast.Expr.t
          }
    [@@deriving sexp_of]

    let constructor id =
      let f payload = Ast.Expr.Construct { constructor = id; payload } in
      Of_expr { priority = Priority.constructor; f }
    ;;

    let func_apply caller =
      let f arg = Ast.Expr.Call { caller; arg } in
      Of_expr { priority = Priority.func_apply; f }
    ;;

    let prefix symbol =
      let f expr = Ast.Expr.Prefix { symbol; t = expr } in
      Of_expr { priority = Util.prefix_priority symbol; f }
    ;;

    let infix expr symbol =
      let f r_expr = Ast.Expr.Infix { left_t = expr; symbol; right_t = r_expr } in
      Of_expr { priority = Util.infix_priority symbol; f }
    ;;
  end

  module Context = struct
    module Component = struct
      module Deep = struct
        type t =
          | Parenthesis
          | Curly_bracket
          | Square_bracket
        [@@deriving equal, sexp_of]
      end

      module Sequential = struct
        type t = Comma [@@deriving equal, sexp_of]
      end

      type t =
        | Deep of Deep.t
        | Sequential of
            { tag : Sequential.t
            ; rev_exprs : Ast.Expr.t list
            }
      [@@deriving sexp_of]

      let deep_tagged_with t ~tag =
        match t with
        | Deep deep -> Deep.equal deep tag
        | Sequential _ -> false
      ;;

      let sequence_rev_exprs t ~tag =
        match t with
        | Sequential { tag = sequential; rev_exprs } when Sequential.equal sequential tag
          -> Some rev_exprs
        | Deep _ | Sequential _ -> None
      ;;
    end

    type t = Component.t list [@@deriving sexp_of]
  end

  type t =
    { stack : Stack_component.t list
    ; context : Context.t
    ; unconsumed_tokens : Token.t Source_position.With_section.t list
    }

  let init ?(context = []) tokens = { stack = []; context; unconsumed_tokens = tokens }
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
  | [ Expr _ ] | [] -> Ok None
  | Of_expr _ :: _ ->
    Or_error.error_s
      [%message "Cannot reduce an of_expression" (stack : State.Stack_component.t list)]
  | Expr _ :: _ ->
    Or_error.error_s
      [%message
        "Invariant broken: Consecutive expressions" (stack : State.Stack_component.t list)]
;;

let reduce_stack_all stack =
  let rec loop = function
    | [ State.Stack_component.Expr expr ] -> Ok expr
    | current_stack ->
      (match%bind.Or_error reduce_stack current_stack with
       | None ->
         Or_error.error_s
           [%message "Failed expression reduction" (stack : State.Stack_component.t list)]
       | Some stack -> loop stack)
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
  | Expr caller :: rest_of_stack ->
    let priority = Priority.func_apply in
    (match%bind.Or_error reduce_stack ~next_priority:priority stack with
     | None ->
       let func_apply = State.Stack_component.func_apply caller in
       Ok (expr :: func_apply :: rest_of_stack)
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
  | Of_expr _ :: _ | [] ->
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
    let priority = Util.infix_priority symbol in
    (match%bind.Or_error reduce_stack ~next_priority:priority stack with
     | None ->
       let infix = State.Stack_component.infix expr symbol in
       Ok (infix :: rest_of_stack)
     | Some reduced_stack -> push_symbol_onto_stack reduced_stack symbol)
;;

(* Sequential contexts are allowed to exist within a deep context, so upon exiting the deep context
   we need to first squash the sequential contexts and put them onto the parse stack.
*)
let rec squash_sequential_contexts ~stack ~context ~unconsumed_tokens =
  match (context : State.Context.t) with
  | Sequential { tag = Comma; rev_exprs } :: rest_of_context ->
    let%bind.Or_error inner_expr = reduce_stack_all stack in
    let tuple =
      let exprs = List.rev (inner_expr :: rev_exprs) in
      Ast.Expr.Tuple { children = exprs }
    in
    let new_stack = [ State.Stack_component.Expr tuple ] in
    squash_sequential_contexts
      ~stack:new_stack
      ~context:rest_of_context
      ~unconsumed_tokens
  | _ -> Ok { State.stack; context; unconsumed_tokens }
;;

(* Exiting a deep context simply expects that top of stack (post sequential context squashing)
   should be that expected deep context.
*)
let exit_deep_context deep_tag ~stack ~context ~unconsumed_tokens =
  let%bind.Or_error { stack; context; unconsumed_tokens } =
    squash_sequential_contexts ~stack ~context ~unconsumed_tokens
  in
  match context with
  | component :: rest_of_context
    when State.Context.Component.deep_tagged_with component ~tag:deep_tag ->
    Ok { State.stack; context = rest_of_context; unconsumed_tokens }
  | context ->
    Or_error.error_s
      [%message
        "Tried escaping non-existent context"
          (deep_tag : State.Context.Component.Deep.t)
          (context : State.Context.t)]
;;

(* Process the resulting state from a "next level" parse (i.e. within parens).

   The result is then appended to the "current level" parse stack.
*)
let process_inner_state (inner_state : State.t) ~stack =
  let%map.Or_error new_stack =
    let%bind.Or_error inner_expr = reduce_stack_all inner_state.stack in
    State.Stack_component.Expr inner_expr |> push_expr_onto_stack stack
  in
  { State.stack = new_stack
  ; context = inner_state.context
  ; unconsumed_tokens = inner_state.unconsumed_tokens
  }
;;

(* Within parens, for example. *)
let parse_inner_state deep_tag ~stack ~context ~unconsumed_tokens ~parse_function =
  let%bind.Or_error inner_state =
    parse_function (State.init ~context:(Deep deep_tag :: context) unconsumed_tokens)
  in
  process_inner_state inner_state ~stack
;;

(* Commas or semicolons, for example.

   If we are already in the proposed sequential context component, append the current expr into it.
   Otherwise, add the proposed sequential context component to the context stack.

   Then, parse the "next level" with the constructed context.

   The result is then appended to the "current level" parse stack.
*)
let parse_sequential_state
      sequential_tag
      ~stack
      ~context
      ~unconsumed_tokens
      ~parse_function
  =
  let%bind.Or_error (next_state : State.t) =
    let%bind.Or_error next_context =
      let%map.Or_error expr = reduce_stack_all stack in
      let aggregated_existing_sequential_context =
        match context with
        | component :: rest_of_context ->
          let%map.Option rev_exprs =
            State.Context.Component.sequence_rev_exprs component ~tag:sequential_tag
          in
          State.Context.Component.Sequential
            { tag = sequential_tag; rev_exprs = expr :: rev_exprs }
          :: rest_of_context
        | [] -> None
      in
      match aggregated_existing_sequential_context with
      | None ->
        State.Context.Component.Sequential { tag = sequential_tag; rev_exprs = [ expr ] }
        :: context
      | Some aggregated_existing_sequential_context ->
        aggregated_existing_sequential_context
    in
    parse_function (State.init ~context:next_context unconsumed_tokens)
  in
  process_inner_state next_state ~stack:[]
;;

let parse tokens =
  let rec loop ({ State.stack; context; unconsumed_tokens } as state) =
    match unconsumed_tokens with
    | token :: rest_of_tokens ->
      let unconsumed_tokens = rest_of_tokens in
      (match (Source_position.With_section.value token : Token.t) with
       | Constant constant ->
         let%bind.Or_error new_stack =
           State.Stack_component.Expr (Constant (Constant constant))
           |> push_expr_onto_stack stack
         in
         loop { State.stack = new_stack; context; unconsumed_tokens }
       | Identifier id ->
         let%bind.Or_error new_stack =
           State.Stack_component.Expr (Id id) |> push_expr_onto_stack stack
         in
         loop { State.stack = new_stack; context; unconsumed_tokens }
       | Big_identifier id ->
         let%bind.Or_error new_stack =
           State.Stack_component.constructor id |> push_expr_onto_stack stack
         in
         loop { State.stack = new_stack; context; unconsumed_tokens }
       | Symbol (Operator symbol) ->
         let%bind.Or_error new_stack = push_symbol_onto_stack stack symbol in
         loop { State.stack = new_stack; context; unconsumed_tokens }
       | Symbol Comma ->
         parse_sequential_state
           Comma
           ~stack
           ~context
           ~unconsumed_tokens
           ~parse_function:loop
       | Grouping (Parenthesis Left) ->
         let%bind.Or_error inner_state =
           parse_inner_state
             Parenthesis
             ~stack
             ~context
             ~unconsumed_tokens
             ~parse_function:loop
         in
         loop inner_state
       | Grouping (Curly_bracket Left) ->
         let%bind.Or_error inner_state =
           parse_inner_state
             Curly_bracket
             ~stack
             ~context
             ~unconsumed_tokens
             ~parse_function:loop
         in
         loop inner_state
       | Grouping (Square_bracket Left) ->
         let%bind.Or_error inner_state =
           parse_inner_state
             Square_bracket
             ~stack
             ~context
             ~unconsumed_tokens
             ~parse_function:loop
         in
         loop inner_state
       | Grouping (Parenthesis Right) ->
         exit_deep_context Parenthesis ~stack ~context ~unconsumed_tokens
       | Grouping (Curly_bracket Right) ->
         exit_deep_context Curly_bracket ~stack ~context ~unconsumed_tokens
       | Grouping (Square_bracket Right) ->
         exit_deep_context Square_bracket ~stack ~context ~unconsumed_tokens
       | Lambda
       | Functor
       | With
       | Symbol _
       | Conditional _
       | Definition _
       | Import _
       | Typeful _ -> Or_error.error_s [%message "Unimplemented"])
    | [] -> Ok state
  in
  let%bind.Or_error { stack; context; unconsumed_tokens } = loop (State.init tokens) in
  let%bind.Or_error { stack; context; unconsumed_tokens } =
    squash_sequential_contexts ~stack ~context ~unconsumed_tokens
  in
  let%bind.Or_error () =
    if List.is_empty context
    then Ok ()
    else
      Or_error.error_s
        [%message
          "Fully parsed token stream but ended with non-empty context"
            (context : State.Context.t)]
  in
  let%bind.Or_error () =
    if List.is_empty unconsumed_tokens
    then Ok ()
    else
      Or_error.error_s
        [%message
          "Fully parsed token stream but ended with non-empty unconsumed tokens"
            (unconsumed_tokens : Token.t Source_position.With_section.t list)]
  in
  reduce_stack_all stack
;;
