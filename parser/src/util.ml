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
  let rec recursive inner_expr =
    match (inner_expr : Ast.Expr.t) with
    | Call { caller = Id id; arg } ->
      let%map.Or_error arg = recursive arg in
      Ast.Binding.Recursive.Function { id; arg }
    | _ ->
      let%map.Or_error pattern = pattern_of_expr inner_expr in
      Ast.Binding.Recursive.Pattern pattern
  in
  match expr with
  | Call { caller = Op { symbol }; arg } ->
    let%map.Or_error arg = recursive arg in
    Ast.Binding.Op_overload { op = symbol; arg }
  | expr ->
    let%map.Or_error binding = recursive expr in
    Ast.Binding.Recursive binding
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
