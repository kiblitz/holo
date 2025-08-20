open! Core

module Left_or_right = struct
  type t =
    | Left
    | Right
  [@@deriving enumerate, sexp_of]
end
