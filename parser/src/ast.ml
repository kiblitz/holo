open! Core
open Import

module Constant = struct
  type t =
    | Constant of Token.Constant.t
    | Unit
  [@@deriving sexp_of]
end

module Pattern = struct
  type t =
    | Unit
    | Id of Token.Identifier.t
    | Construct of
        { constructor : Token.Big_identifier.t
        ; payload : t
        }
  [@@deriving sexp_of]
end

module Value = struct
  type t =
    | Id of Token.Identifier.t
    | Function of
        { id : Token.Identifier.t
        ; args : Pattern.t Nonempty_list.t
        }
  [@@deriving sexp_of]
end

module Binding = struct
  type t =
    | Pattern of Pattern.t
    | Value of Value.t
  [@@deriving sexp_of]
end

module Expr = struct
  type t =
    | Id of Token.Identifier.t
    | Constant of Constant.t
    | Call of
        { caller : t
        ; arg : t
        }
    | Construct of
        { constructor : Token.Big_identifier.t
        ; payload : t
        }
    | Prefix of
        { symbol : Token.Symbol.Operator.Base.t Nonempty_list.t
        ; t : t
        }
    | Infix of
        { symbol : Token.Symbol.Operator.t
        ; left_t : t
        ; right_t : t
        }
    | Component of
        { t : t
        ; child : Token.Identifier.t
        }
    | Tuple of { children : t list }
    | List of { children : t list }
    | With_type of
        { t : t
        ; type_ : t
        }
    | Concat of
        { t1 : t
        ; t2 : t
        }
    | Scope of
        { binding : Binding.t
        ; to_ : t
        ; in_ : t
        }
    | If of
        { condition : t
        ; then_ : t
        ; else_ : t option
        }
  [@@deriving sexp_of]
end

type t =
  | Expr of Expr.t
  | Program of { toplevel : Expr.t list }
[@@deriving sexp_of]
