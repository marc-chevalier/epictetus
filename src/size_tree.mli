[@@@warning "+A"]

(** The structure of a tree.
    A tree_size is consistent if all integers are non negative. Moreover the sum
    of sizes of the children of a node must be lower than or equal to the size
    of the node.

    When printing with a tree_size, each element of the tree_contents must have
    a length lower than or equal to the size in the tree_size.

    Every generated tree_size satisfy this constraints. Be careful if you want
    to use them manually.
*)
type t = {
  width: int;
  children: t list;
}

(** Compute the join of given tree_size. Intended for internal use. Be careful.
*)
val merge: t -> t -> t
