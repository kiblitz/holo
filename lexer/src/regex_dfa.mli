open! Core

module Config : sig
  type t =
    | Empty
    | Epsilon
    | Char of char
    | Concat of t * t
    | Or of t * t
    | Star of t

  val plus : t -> t
  val opt : t -> t
  val exact : string -> t
  val char_or : char list -> t
  val concat : t list -> t
  val or_ : t list -> t

  module For_testing : sig
    val sexp_of_t : t -> Sexp.t
    val reduce : t -> t
    val ( mod ) : t -> char -> t
    val possible_chars : t -> Char.Set.t
  end
end
