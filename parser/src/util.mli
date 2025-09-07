open! Core
open Import

val validate_unconsumed_tokens_is_empty
  :  Token.t Source_position.With_section.t list
  -> unit Or_error.t
