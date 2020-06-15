[@@@warning "+A"]

type alignment =
  | Left
  | Right
  | Center

module type CONTENT =
  (sig
    type contents
    val contents_length: contents -> int
    val pp: Format.formatter -> contents -> unit
  end)

module type ALIGNER =
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
    val tree_size : tree_contents -> Size_tree.t

    (** Print a tree_contents with respect to a given tree_size. This tree_size
        must be bigger than the tree_size corresponding to the tree_contents.
        Otherwise the behavior is undefined. Intended for internal use. Be
        careful.
    *)
    val print_tree_with_size: Size_tree.t -> Format.formatter -> tree_contents -> unit

    (** Print a list of tree_contents in a consistent way: it use the smallest
        tree_size computed by joining all tree_size. This function must compute
        and merge all tree_size before starting. This is the default entrypoint.
    *)
    val print_table: Format.formatter -> tree_contents list -> unit
  end)
