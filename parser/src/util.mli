open! Core
open Import

val binding_of_expr : Ast.Expr.t -> Ast.Binding.t Or_error.t

val validate_unconsumed_tokens_is_empty
  :  Token.t Source_position.With_section.t list
  -> unit Or_error.t
