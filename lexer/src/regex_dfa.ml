open! Core

module Config = struct
  type t =
    | Empty
    | Epsilon
    | Char of char
    | Concat of t * t
    | Or of t * t
    | Star of t
  [@@deriving sexp_of]

  let is_empty = function
    | Empty -> true
    | _ -> false
  ;;

  let is_epsilon = function
    | Epsilon -> true
    | _ -> false
  ;;

  let rec contains_epsilon = function
    | Empty -> false
    | Epsilon -> true
    | Char _ -> false
    | Concat (t1, t2) -> contains_epsilon t1 && contains_epsilon t2
    | Or (t1, t2) -> contains_epsilon t1 || contains_epsilon t2
    | Star _ -> true
  ;;

  let rec reduce t =
    match t with
    | Empty | Epsilon | Char _ -> t
    | Concat (t1, t2) ->
      let rt1 = reduce t1 in
      let rt2 = reduce t2 in
      if is_empty rt1 || is_empty rt2
      then Empty
      else if is_epsilon rt1
      then rt2
      else if is_epsilon rt2
      then rt1
      else Concat (rt1, rt2)
    | Or (t1, t2) ->
      let rt1 = reduce t1 in
      let rt2 = reduce t2 in
      if is_empty rt1 then rt2 else if is_empty rt2 then rt1 else Or (rt1, rt2)
    | Star t ->
      let rt = reduce t in
      if is_empty rt || is_epsilon rt then Epsilon else Star rt
  ;;

  let rec ( mod ) t (c : char) =
    match reduce t with
    | Empty -> Empty
    | Epsilon -> Empty
    | Char c_ when Char.(c = c_) -> Epsilon
    | Char _ -> Empty
    | Concat (t1, t2) ->
      let t1_mod_c_t2 = Concat (t1 mod c, t2) in
      if not (contains_epsilon t1) then t1_mod_c_t2 else Or (t1_mod_c_t2, t2 mod c)
    | Or (t1, t2) -> Or (t1 mod c, t2 mod c)
    | Star t -> Concat (t mod c, Star t)
  ;;

  let plus t = Concat (t, Star t)
  let opt t = Or (t, Epsilon)

  let builder lst ~leaf ~join ~default =
    let rec loop = function
      | x :: xs ->
        (match loop xs with
         | None -> leaf x
         | Some t -> join (leaf x) t)
        |> Some
      | [] -> None
    in
    loop lst |> Option.value ~default
  ;;

  let exact str =
    builder
      (String.to_list str)
      ~leaf:(fun c -> Char c)
      ~join:(fun t1 t2 -> Concat (t1, t2))
      ~default:Epsilon
  ;;

  let char_or cs =
    builder cs ~leaf:(fun c -> Char c) ~join:(fun t1 t2 -> Or (t1, t2)) ~default:Empty
  ;;

  let concat ts =
    builder ts ~leaf:Fn.id ~join:(fun t1 t2 -> Concat (t1, t2)) ~default:Epsilon
  ;;

  let or_ ts = builder ts ~leaf:Fn.id ~join:(fun t1 t2 -> Or (t1, t2)) ~default:Empty

  module For_testing = struct
    let reduce = reduce
    let ( mod ) = ( mod )
    let sexp_of_t = sexp_of_t
  end
end
