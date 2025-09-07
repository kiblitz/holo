open! Core
open Import

module State = struct
  type t =
    { rev_toplevel_exprs : Ast.Expr.t list
    ; unconsumed_tokens : Token.t Source_position.With_section.t list
    }

  let init tokens = { rev_toplevel_exprs = []; unconsumed_tokens = tokens }
end

let parse tokens =
  let rec loop { State.rev_toplevel_exprs; unconsumed_tokens } =
    let%bind.Or_error { Expr_parser.Result.expr; unconsumed_tokens } =
      Expr_parser.parse_raw unconsumed_tokens
    in
    let state =
      { State.rev_toplevel_exprs = expr :: rev_toplevel_exprs; unconsumed_tokens }
    in
    match List.is_empty unconsumed_tokens with
    | true -> Ok state
    | false -> loop state
  in
  let%bind.Or_error { State.rev_toplevel_exprs; unconsumed_tokens } =
    loop (State.init tokens)
  in
  let%map.Or_error () = Util.validate_unconsumed_tokens_is_empty unconsumed_tokens in
  { Ast.toplevel = List.rev rev_toplevel_exprs }
;;
