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

    val tree_size : tree_contents -> Size_tree.t
    val print_tree_with_size: Size_tree.t -> Format.formatter -> tree_contents -> unit
    val print_table: Format.formatter -> tree_contents list -> unit
  end)
