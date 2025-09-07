open! Core
open Import

let validate_unconsumed_tokens_is_empty unconsumed_tokens =
  if List.is_empty unconsumed_tokens
  then Ok ()
  else
    Or_error.error_s
      [%message
        "Fully parsed token stream but ended with non-empty unconsumed tokens"
          (unconsumed_tokens : Token.t Source_position.With_section.t list)]
;;
