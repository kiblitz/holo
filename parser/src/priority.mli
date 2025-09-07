open! Core
open Import

type t [@@deriving sexp_of]

val submodule : t
val non_neg_prefix : t
val constructor : t
val func_apply : t
val neg : t
val pow : t
val times : t
val percent : t
val divide : t
val at : t
val caret : t
val plus : t
val minus : t
val cmp : t
val and_ : t
val or_ : t
val prefix : Token.Symbol.Operator.Base.t Nonempty_list.t -> t
val infix : Token.Symbol.Operator.t -> t

module Comparison_result : sig
  type t =
    | Build
    | Reduce
  [@@deriving equal]
end

val compare : left:t -> right:t -> Comparison_result.t
