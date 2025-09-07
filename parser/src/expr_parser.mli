open! Core
open Import

module Result : sig
  type t =
    { expr : Ast.Expr.t
    ; unconsumed_tokens : Token.t Source_position.With_section.t list
    }
end

val parse_raw : Token.t Source_position.With_section.t list -> Result.t Or_error.t
val parse : Token.t Source_position.With_section.t list -> Ast.Expr.t Or_error.t
