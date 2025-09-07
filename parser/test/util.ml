open! Core
open Import

let parse_expr text =
  let%map.With_errors tokens = Lexer.lex text ~filename:"file.txt" in
  Parser.Expr.parse tokens
;;
