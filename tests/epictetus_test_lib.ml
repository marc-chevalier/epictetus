open Epictetus

let rec pp_print_list (pp_v: Format.formatter -> 'a -> unit) (ppf: Format.formatter) (l: 'a list) : unit =
  match l with
  | [] -> ()
  | [v] -> pp_v ppf v
  | v :: vs ->
    pp_v ppf v;
    Format.pp_print_string ppf "; ";
    pp_print_list pp_v ppf vs

let rec pp_string_tree_contents (fmt: Format.formatter) (t: Std_aligners.StringAligner.tree_contents) : unit =
  match t with
  | {node=Leaf s; _} -> Format.pp_print_string fmt s
  | {node=Node l; _} -> Format.fprintf fmt "[%a]" (pp_print_list pp_string_tree_contents) l

let rec pp_string_tree_size (fmt: Format.formatter) (t: Size_tree.t) : unit =
  Format.fprintf fmt "%d[%a]" t.width (pp_print_list pp_string_tree_size) t.children

let rec has_same_shape (c: Std_aligners.StringAligner.tree_contents) (s: Size_tree.t) : bool =
  match c.node, s with
  | Leaf _, {children=[]; _} -> true
  | Leaf _, _ -> false
  | Node c, {children=s; _} ->
    List.(length c = length s && for_all2 has_same_shape c s)

let rec consistent_tree_size (size: Size_tree.t) : bool =
  size.width >= 0
  && List.fold_left (fun acc node -> acc + node.Size_tree.width) 0 size.Size_tree.children <= size.width
  && List.for_all consistent_tree_size size.children

let rec subseteq (a: Size_tree.t) (b: Size_tree.t) : bool =
  let rec aux a b =
    match a, b with
    | [], _ -> true
    | _ :: _, [] -> false
    | ha :: ta, hb :: tb ->
      subseteq ha hb && aux ta tb
  in
  a.width <= b.width && aux a.children b.children

let sleaf (width: int) : Size_tree.t =
  {width; children = []}

let snode (children: Size_tree.t list) (width: int) : Size_tree.t =
  {children; width}
