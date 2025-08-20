open! Core

module Left_or_right : sig
  type t =
    | Left
    | Right
  [@@deriving enumerate, sexp_of]
end
