open! Core

module T : sig
  type t [@@deriving sexp_of]

  val create : level:int -> on_ties:Holo_util.Left_or_right.t -> t
  val level : t -> int
  val on_ties : t -> Holo_util.Left_or_right.t
end = struct
  type t =
    { (* Higher value means higher priority *)
      level : int
    ; on_ties : Holo_util.Left_or_right.t
    }
  [@@deriving equal, fields ~getters, sexp_of]

  let create =
    let register = Int.Table.create () in
    fun ~level ~on_ties ->
      let t = { level; on_ties } in
      let cached = Hashtbl.find_or_add register level ~default:(fun () -> t) in
      if equal t cached
      then t
      else
        raise_s
          [%message
            "Cannot create multiple distinct priorities with the same level"
              (t : t)
              (cached : t)]
  ;;
end

include T

let submodule = create ~level:12 ~on_ties:Left
let prefix = create ~level:11 ~on_ties:Left
let constructor = create ~level:10 ~on_ties:Right
let func_apply = create ~level:9 ~on_ties:Left
let neg = create ~level:8 ~on_ties:Left
let pow = create ~level:7 ~on_ties:Right

let times, percent, divide, at =
  let t = create ~level:6 ~on_ties:Left in
  t, t, t, t
;;

let caret = create ~level:5 ~on_ties:Right

let plus, minus =
  let t = create ~level:4 ~on_ties:Left in
  t, t
;;

let cmp = create ~level:3 ~on_ties:Left
let and_ = create ~level:2 ~on_ties:Right
let or_ = create ~level:1 ~on_ties:Right

module Comparison_result = struct
  type t =
    | Build
    | Reduce
  [@@deriving equal]
end

let compare ~left ~right =
  if level left > level right
  then Comparison_result.Reduce
  else if level left < level right
  then Comparison_result.Build
  else (
    match (on_ties left : Holo_util.Left_or_right.t) with
    | Left -> Comparison_result.Reduce
    | Right -> Comparison_result.Build)
;;
