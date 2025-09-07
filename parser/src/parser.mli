open! Core
open Import

module Expr : sig
  val parse : Token.t Source_position.With_section.t list -> Ast.Expr.t Or_error.t
end

module Program : sig
  val parse : Token.t Source_position.With_section.t list -> Ast.t Or_error.t
end
