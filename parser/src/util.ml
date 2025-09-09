open! Core
open Import

let pattern_of_expr expr =
  let rec loop inner_expr =
    match (inner_expr : Ast.Expr.t) with
    | Underscore -> Ok Ast.Pattern.Underscore
    | Id id -> Ok (Ast.Pattern.Id id)
    | Constant Unit -> Ok Unit
    | Variant { tag; payload } ->
      (match payload with
       | None -> Ok (Variant { tag; payload = None })
       | Some payload ->
         let%map.Or_error payload = loop payload in
         Ast.Pattern.Variant { tag; payload = Some payload })
    | _ ->
      Or_error.error_s
        [%message "Failed to convert expression to a binding" (expr : Ast.Expr.t)]
  in
  loop expr
;;

let binding_of_expr (expr : Ast.Expr.t) =
  let module Function_or_op_overload = struct
    type t =
      | Function of Token.Identifier.t
      | Op_overload of Token.Symbol.Operator.t
  end
  in
  let rec get_args (inner_expr : Ast.Expr.t) =
    match inner_expr with
    | Call { caller = Id id; arg } ->
      let%map.Or_error arg = pattern_of_expr arg in
      Function_or_op_overload.Function id, Nonempty_list.singleton arg
    | Call { caller = Op { symbol }; arg } ->
      let%map.Or_error arg = pattern_of_expr arg in
      Function_or_op_overload.Op_overload symbol, Nonempty_list.singleton arg
    | Call { caller; arg } ->
      let%bind.Or_error function_or_op_overload, args = get_args caller in
      let%map.Or_error arg = pattern_of_expr arg in
      function_or_op_overload, Nonempty_list.cons arg args
    | _ ->
      Or_error.error_s [%message "Cannot get args for a non-call" (expr : Ast.Expr.t)]
  in
  match expr with
  | Call _ ->
    (match%map.Or_error get_args expr with
     | Function_or_op_overload.Function id, rev_args ->
       Ast.Binding.Function { id; args = Nonempty_list.reverse rev_args }
     | Op_overload op, rev_args ->
       Ast.Binding.Op_overload { op; args = Nonempty_list.reverse rev_args })
  | expr ->
    let%map.Or_error pattern = pattern_of_expr expr in
    Ast.Binding.Pattern pattern
;;

let validate_unconsumed_tokens_is_empty unconsumed_tokens =
  if List.is_empty unconsumed_tokens
  then Ok ()
  else
    Or_error.error_s
      [%message
        "Fully parsed token stream but ended with non-empty unconsumed tokens"
          (unconsumed_tokens : Token.t Source_position.With_section.t list)]
;;
