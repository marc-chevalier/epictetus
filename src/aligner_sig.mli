[@@@warning "+A"]

module type CONTENTS =
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
      align: Alignment.alignment;
    }

    val leaf: ?fill_with:char -> ?align:Alignment.alignment -> contents -> tree_contents
    val node: ?fill_with:char -> ?align:Alignment.alignment -> tree_contents list -> tree_contents

    (** Compute the tree_size of a tree_contents.
    *)
    val tree_size : tree_contents -> Size_tree.t

    (** Compute the tree_size of a list of tree_contents. Return None if the list is empty.
    *)
    val tree_size_of_list : tree_contents list -> Size_tree.t option

    (** Print a tree_contents with respect to a given tree_size. This tree_size
        must be bigger than the tree_size corresponding to the tree_contents.
        Otherwise the behavior is undefined. Be careful.
    *)
    val print_tree_with_size: Size_tree.t -> Format.formatter -> tree_contents -> unit

    (** Print a list of tree_contents in a consistent way: it uses the smallest
        tree_size computed by joining all tree_size. This function must compute
        and merge all tree_size before starting. This is a good entrypoint.
    *)
    val print_table: Format.formatter -> tree_contents list -> unit

    (** Like print_table but call the first parameter at the end of each line.
    *)
    val kprint_table:  (Format.formatter -> unit) -> Format.formatter -> tree_contents list -> unit

    (** Print in a list of string a list of tree_contents in a consistent way:
        it uses the smallest tree_size computed by joining all tree_size. This
        function must compute and merge all tree_size before starting. This is
        a good entrypoint.
    *)
    val stringify_table: tree_contents list -> string list

    (* Like stringify_table but returns a pretty-printer, rather than a string
       for each line. It's the lowest level function behind all other printing
       functions. *)
    val pp_of_table: tree_contents list -> (Format.formatter -> unit) list
  end)
