open! Core

module Identifier = struct
  type t = { name : string } [@@deriving sexp_of]
end

module With_transformation = struct
  type t = Identifier.t option [@@deriving sexp_of]
end

module Constant = struct
  type t =
    | Bool of bool
    | Int of string
    | Float of string
    | Char of string
    | String of string
  [@@deriving sexp_of]
end

module Symbol = struct
  module Base = struct
    type t =
      | Equal
      | Tilda
      | At
      | Caret
      | Pipe
      | Ampersand
      | Plus
      | Minus
      | Times
      | Div
      | Dollar
      | Percent
      | Greater
      | Less
    [@@deriving sexp_of]
  end

  module Non_custom = struct
    type t =
      | Dot
      | Colon
      | Semicolon
      | Comma
      | Walrus
    [@@deriving sexp_of]
  end

  type t =
    | Base of Base.t Nonempty_list.t
    | Non_custom of Non_custom.t
  [@@deriving sexp_of]
end

module Import = struct
  type t =
    | Open of { allow_unused : bool }
    | Include
  [@@deriving sexp_of]
end

module Definition = struct
  type t =
    | Assign_with_transformation of Identifier.t
    | Rec
    | And
    | In
    | Module of With_transformation.t
    | Sig
    | Struct
    | End
    | Val
  [@@deriving sexp_of]
end

module Conditional = struct
  type t =
    | If of With_transformation.t
    | Then
    | Else
    | Match of With_transformation.t
    | When
    | Function
  [@@deriving sexp_of]
end

module Typeful = struct
  type t =
    | Type
    | As
    | Of
    | Mutable
    | Nonrec
  [@@deriving sexp_of]
end

module Grouping = struct
  type t =
    | Parenthesis of Util.Left_or_right.t
    | Curly_bracket of Util.Left_or_right.t
    | Square_bracket of Util.Left_or_right.t
  [@@deriving sexp_of]
end

type t =
  | Constant of Constant.t
  | Identifier of Identifier.t
  | Symbol of Symbol.t
  | Definition of Definition.t
  | Conditional of Conditional.t
  | Typeful of Typeful.t
  | Grouping of Grouping.t
  | Lambda
  | Functor
  | With
[@@deriving sexp_of]
