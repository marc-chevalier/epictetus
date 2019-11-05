open Epictetus.Tabulator

let rec pp_print_list (pp_v: Format.formatter -> 'a -> unit) (ppf: Format.formatter) (l: 'a list) : unit =
  match l with
  | [] -> ()
  | [v] -> pp_v ppf v
  | v :: vs ->
    pp_v ppf v;
    Format.pp_print_string ppf "; ";
    pp_print_list pp_v ppf vs

let rec pp_string_tree_contents (fmt: Format.formatter) (t: StringTabulator.tree_contents) : unit =
  match t with
  | {node=Leaf s; _} -> Format.pp_print_string fmt s
  | {node=Node l; _} -> Format.fprintf fmt "[%a]" (pp_print_list pp_string_tree_contents) l

let rec pp_string_tree_size (fmt: Format.formatter) (t: tree_size) : unit =
  Format.fprintf fmt "%d[%a]" t.width (pp_print_list pp_string_tree_size) t.children

let rec has_same_shape (c: StringTabulator.tree_contents) (s: tree_size) : bool =
  match c.node, s with
  | Leaf _, {children=[]; _} -> true
  | Leaf _, _ -> false
  | Node c, {children=s; _} ->
    List.(length c = length s && for_all2 has_same_shape c s)

let rec consistent_tree_size (size: tree_size) : bool =
  size.width >= 0
  && List.fold_left (fun acc node -> acc + node.width) 0 size.children <= size.width
  && List.for_all consistent_tree_size size.children

let rec subseteq (a: tree_size) (b: tree_size) : bool =
  let rec aux a b =
    match a, b with
    | [], _ -> true
    | _ :: _, [] -> false
    | ha :: ta, hb :: tb ->
      subseteq ha hb && aux ta tb
  in
  a.width <= b.width && aux a.children b.children

let sleaf (width: int) : tree_size =
  {width; children = []}

let snode (children: tree_size list) (width: int) : tree_size =
  {children; width}
