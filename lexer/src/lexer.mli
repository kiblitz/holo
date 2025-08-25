open! Core

val lex
  :  string
  -> filename:string
  -> Token.t Source_position.With_section.t list With_errors.t
