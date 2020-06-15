[@@@warning "+A"]

type t = {
  width: int;
  children: t list;
}

let rec merge (a: t) (b: t) : t =
  let rec aux (a: t list) (b: t list) : t list =
    match a, b with
    | a, [] | [], a -> a
    | t1::q1, t2::q2 -> merge t1 t2::aux q1 q2
  in
  let children = aux a.children b.children in
  {children; width = max (max a.width b.width) (List.fold_left (fun a node -> a + node.width) 0 children)}
