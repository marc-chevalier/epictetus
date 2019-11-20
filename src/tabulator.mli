[@@@warning "+A"]

(** Thrown when a printing error happens. Using the standard entry_point
    [print_table] it shouldn't be possible. However, it is when using other
    functions without care. Please note that this is not the only exception that
    can be raised. For instance Invalid_argument is also possible.
*)
exception PrintError of string list

type alignment =
  | Left
  | Right
  | Center

(** The structure of a tree.
    A tree_size is consistent if all integers are non negative. Moreover the sum
    of sizes of the children of a node must be lower than or equal to the size
    of the node.

    When printing with a tree_size, each element of the tree_contents must have
    a length lower than or equal to the size in the tree_size.

    Every generated tree_size satisfy this constraints. Be careful if you want
    to use them manually.
*)
type tree_size = {
  width: int;
  children: tree_size list;
}

(** Compute the join of given tree_size. Intended for internal use. Be careful.
*)
val merge_size_trees: tree_size -> tree_size -> tree_size

module type PARAM =
  (sig
    type contents
    val contents_length: contents -> int
    val pp: Format.formatter -> contents -> unit
  end)

module type TABULATOR =
  (sig
    type contents

    type _tree_contents =
      | Leaf of contents
      | Node of tree_contents list

    and tree_contents = {
      node: _tree_contents;
      fill_with: char;
      align: alignment;
    }

    val leaf: ?fill_with:char -> ?align:alignment -> contents -> tree_contents
    val node: ?fill_with:char -> ?align:alignment -> tree_contents list -> tree_contents

    (** Compute the tree_size of a tree_contents. Intended for internal use.
        Cannot be very bad anyway.
    *)
    val tree_size : tree_contents -> tree_size

    (** Print a tree_contents with respect to a given tree_size. This tree_size
        must be bigger than the tree_size corresponding to the tree_contents.
        Otherwise the behavior is undefined. Intended for internal use. Be
        careful.
    *)
    val print_tree_with_size: tree_size -> Format.formatter -> tree_contents -> unit

    (** Print a list of tree_contents in a consistent way: it use the smallest
        tree_size computed by joining all tree_size. This function must compute
        and merge all tree_size before starting. This is the default entrypoint.
    *)
    val print_table: Format.formatter -> tree_contents list -> unit
  end)

(** Build a TABULATOR given a PARAM module.
*)
module Tabulator (T: PARAM) : TABULATOR with type contents = T.contents

(** A default TABULATOR in which contents are pretty-printers with the expected
    output size. Especially useful when using semantic tags or when printing
    ANSI escape sequences.
*)
module SizedPPTabulator : TABULATOR with type contents = (Format.formatter -> unit) * int

(** A default TABULATOR in which contents are pretty-printers with the expected
    output size. Especially useful when using semantic tags or when printing
    ANSI escape sequences. The size is computed by calling the pretty-printer.
    It would better be pure!
*)
module PPTabulator : TABULATOR with type contents = Format.formatter -> unit

(** A default TABULATOR in which contents are string. Very straightforward.
*)
module StringTabulator : TABULATOR with type contents = string
