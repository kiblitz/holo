open! Core

module Config = struct
  module T = struct
    type t =
      | Empty
      | Epsilon
      | Char of char
      | Concat of t * t
      | Or of t * t
      | Star of t
    [@@deriving compare, hash, sexp_of]
  end

  include T
  include Hashable.Make_plain (T)

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
    match t with
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

  let rec possible_chars = function
    | Empty | Epsilon -> Char.Set.empty
    | Char c -> Char.Set.singleton c
    | Concat (t1, t2) | Or (t1, t2) ->
      Core.Set.union (possible_chars t1) (possible_chars t2)
    | Star t -> possible_chars t
  ;;

  module For_testing = struct
    let sexp_of_t = sexp_of_t
    let reduce = reduce
    let ( mod ) = ( mod )
    let possible_chars = possible_chars
  end
end

module Accepting_state_metadata = struct
  type 'a t = { cont_of_match : string -> 'a } [@@deriving sexp_of]
end

type 'a t =
  { id : int
  ; mutable next_nodes : 'a t Char.Map.t
  ; mutable accepting_state_metadata : 'a Accepting_state_metadata.t option
  }
[@@deriving sexp_of]

let next_id = ref 0

let make ?(next_nodes = Char.Map.empty) ?(accepting_state_metadata = None) () =
  let curr_id = !next_id in
  next_id := !next_id + 1;
  { id = curr_id; next_nodes; accepting_state_metadata }
;;

let create (type a) (config : Config.t) ~(cont_of_match : string -> a) : a t =
  let config = Config.reduce config in
  let memo_table = Config.Table.create () in
  let char_universe = Config.possible_chars config in
  let rec loop (config : Config.t) =
    Hashtbl.find_or_add memo_table config ~default:(fun () ->
      let t = make () in
      Hashtbl.set memo_table ~key:config ~data:t;
      (* To avoid infinite looping, we mark this config as visited *)
      let next_nodes =
        Char.Map.of_key_set char_universe ~f:(fun c ->
          let next_config = Config.(config mod c |> reduce) in
          if Config.is_empty next_config then None else Some (loop next_config))
        |> Map.filter_map ~f:Fn.id
      in
      let accepting_state_metadata =
        if not (Config.contains_epsilon config)
        then None
        else Some { Accepting_state_metadata.cont_of_match }
      in
      t.next_nodes <- next_nodes;
      t.accepting_state_metadata <- accepting_state_metadata;
      t)
  in
  loop config
;;

module For_testing = struct
  let sexp_of_t (type a) (sexp_of_a : a -> Sexp.t) (t : a t) =
    let module Node = struct
      type t =
        | Node of { next_nodes : int Char.Map.t }
        | Accept of
            { next_nodes : int Char.Map.t
            ; accepting_state_metadata : a Accepting_state_metadata.t
            }
      [@@deriving sexp_of]
    end
    in
    let node_of_t { id = _; next_nodes; accepting_state_metadata } =
      let next_nodes = Map.map next_nodes ~f:(fun t -> t.id) in
      match accepting_state_metadata with
      | None -> Node.Node { next_nodes }
      | Some accepting_state_metadata ->
        Node.Accept { next_nodes; accepting_state_metadata }
    in
    let graph = Int.Table.create () in
    let rec loop t =
      let id = t.id in
      match Hashtbl.find graph id with
      | Some _ -> ()
      | None ->
        let next_nodes = t.next_nodes in
        Hashtbl.set graph ~key:id ~data:(node_of_t t);
        Map.iter next_nodes ~f:loop
    in
    loop t;
    Int.Table.sexp_of_t Node.sexp_of_t graph
  ;;
end
