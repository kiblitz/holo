(* TODO: This should be upstreamed *)
open! Core

type 'a t [@@deriving sexp_of]

include Monad.S with type 'a t := 'a t

val error_s : 'a -> Sexp.t -> 'a t
