open! Core

type t =
  | Left
  | Right
[@@deriving enumerate, equal, sexp_of]
