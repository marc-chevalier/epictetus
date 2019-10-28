[@@@warning "+A"]
exception PrintError of string list

type tree_size =
  | SLeaf of int
  | SNode of tree_size list * int

let rec merge_size_trees (a: tree_size) (b: tree_size) : tree_size =
  let rec aux (a: tree_size list) (b: tree_size list) : tree_size list =
    match a, b with
    | a, [] | [], a -> a
    | (t1::q1), (t2::q2) -> merge_size_trees t1 t2::aux q1 q2
  in
  match a, b with
  | SLeaf p,      SLeaf q -> SLeaf (max p q)
  | SNode (l, p), SLeaf q -> SNode (l, max p q)
  | SLeaf p, SNode (m, q) -> SNode (m, max p q)
  | SNode (l, p), SNode (m, q) -> let a = aux l m in
    SNode (a, max (max p q) (List.fold_left (fun a -> function SLeaf n | SNode(_, n) -> a + n) 0 a))

module type PARAM =
  (sig
    type contents
    val contents_length: contents -> int
    val pp: Format.formatter -> contents -> unit
  end)

module type TABULATOR =
  (sig
    type contents

    type tree_contents =
      | Leaf of contents
      | Node of tree_contents list

    val tree_size : tree_contents -> tree_size
    val print_tree_with_size: tree_size -> Format.formatter -> tree_contents -> unit
    val print_table: Format.formatter -> tree_contents list -> unit
  end)

module Tabulator (T: PARAM)
  : TABULATOR
    with type contents = T.contents
  =
  (struct
    type contents = T.contents

    type tree_contents =
      | Leaf of contents
      | Node of tree_contents list

    let rec tree_size : tree_contents -> tree_size = function
      | Leaf s -> SLeaf (T.contents_length s)
      | Node l -> let size_l = List.map tree_size l in SNode (size_l, List.fold_left (fun a -> function | SLeaf n | SNode(_, n) -> a+n ) 0 size_l)

    let print_tree_with_size (size: tree_size) (fmt: Format.formatter) (str: tree_contents) : unit =
      let pad (fmt: Format.formatter) (n: int) : unit =
        Format.pp_print_string fmt (String.make n ' ')
      in
      let rec aux (str: tree_contents) (size: tree_size) : int =
        match str, size with
        | Leaf s, SLeaf n
        | Leaf s, SNode (_, n) -> let d = n - T.contents_length s in
          let () = Format.fprintf fmt "%a%a" T.pp s pad d in
          T.contents_length s + d
        | Node _, SLeaf _ -> raise (PrintError [__LOC__; Format.asprintf "print_tree_with_size: string sub-tree with no size sub-tree"])
        | Node l, SNode (m, n) ->
          let rec aux2 l m : int =
            match l, m with
            | [], _ -> let () = Format.pp_print_string fmt "" in 0
            | t1::q1, t2::q2 -> let b = aux t1 t2 in let d = aux2 q1 q2 in b + d
            | _::_, [] -> raise (PrintError [__LOC__; Format.asprintf "print_tree_with_size: pattern inconsistent with string tree"])
          in
          let size_s = aux2 l m in
          let d = n - size_s in
          let () = pad fmt d in
          size_s + d
      in
      aux str size |> ignore

    let print_table (fmt: Format.formatter) (str_tree : tree_contents list) : unit =
      let size_trees : tree_size list = List.map tree_size str_tree in
      match size_trees with
      | [] -> ()
      | h::t ->
        let size_tree : tree_size = List.fold_left merge_size_trees h t in
        let pp = print_tree_with_size size_tree in
        let () = List.iter (fun x -> Format.fprintf fmt "%a\n" pp x) str_tree in
        ()

  end)

module Sized_pp_param
  : PARAM
    with type contents = (Format.formatter -> unit) * int
  =
  (struct
    type contents = (Format.formatter -> unit) * int
    let contents_length : contents -> int = snd
    let pp (fmt: Format.formatter) (pp, _: contents) : unit = pp fmt
  end)

module SizedPPTabulator = Tabulator(Sized_pp_param)

module Pp_param
  : PARAM
    with type contents = Format.formatter -> unit
  =
  (struct
    type contents = Format.formatter -> unit
    let contents_length (c: contents) : int = Format.asprintf "%t" c |> String.length
    let pp (fmt: Format.formatter) (pp: contents) : unit = pp fmt
  end)

module PPTabulator = Tabulator(Pp_param)

module String_param
  : PARAM
    with type contents = string
  =
  (struct
    type contents = string
    let contents_length : string -> int = String.length
    let pp (fmt: Format.formatter) (c : contents) : unit = Format.pp_print_string fmt c
  end)

module StringTabulator = Tabulator(String_param)
