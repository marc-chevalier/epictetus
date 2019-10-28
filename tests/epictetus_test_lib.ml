open Epictetus.Tabulator

let rec pp_string_tree_contents (fmt: Format.formatter) (t: StringTabulator.tree_contents) : unit =
  match t with
  | Leaf s -> Format.pp_print_string fmt s
  | Node l -> Ocolor_format.pp_list pp_string_tree_contents fmt l

let rec pp_string_tree_size (fmt: Format.formatter) (t: tree_size) : unit =
  match t with
  | SLeaf s -> Format.pp_print_int fmt s
  | SNode (l, s) -> Format.fprintf fmt "%d%a" s (Ocolor_format.pp_list pp_string_tree_size) l

let rec has_same_shape (c: StringTabulator.tree_contents) (s: tree_size) : bool =
  match c, s with
  | Leaf _, SLeaf _ -> true
  | Node _, SLeaf _ | Leaf _, SNode _ -> false
  | Node c, SNode (s, _) ->
    List.(length c = length s) && List.for_all2 has_same_shape c s

let rec consistent_tree_size (s: tree_size) : bool =
  match s with
  | SLeaf s -> s >= 0
  | SNode (l, s) ->
    List.fold_left (fun acc -> function SLeaf s | SNode (_, s) -> acc + s) 0 l <= s
    && List.for_all consistent_tree_size l

let rec subseteq (a: tree_size) (b: tree_size) : bool =
  match a, b with
  | SLeaf _, SLeaf _ -> true
  | SNode _, SLeaf _ -> false
  | SLeaf _, SNode _ -> true
  | SNode (la, sa), SNode (lb, sb) ->
    let rec aux a b =
      match a, b with
      | [], _ -> true
      | _ :: __, [] -> false
      | ha :: ta, hb :: tb ->
        subseteq ha hb && aux ta tb
    in
    sa <= sb && aux la lb
