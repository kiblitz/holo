open! Core
open Import

val parse_expr : string -> Ast.Expr.t Or_error.t With_errors.t
val parse_program : string -> Ast.t Or_error.t With_errors.t
