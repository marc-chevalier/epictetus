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

    val tree_size : tree_contents -> Size_tree.t
    val tree_size_of_list : tree_contents list -> Size_tree.t option
    val print_tree_with_size: ?trailing_whitespaces:bool -> Size_tree.t -> Format.formatter -> tree_contents -> unit

    val print_table: ?trailing_whitespaces:bool -> Format.formatter -> tree_contents list -> unit
    val kprint_table:  ?trailing_whitespaces:bool -> (Format.formatter -> unit) -> Format.formatter -> tree_contents list -> unit
    val stringify_table: ?trailing_whitespaces:bool -> tree_contents list -> string list
    val pp_of_table: ?trailing_whitespaces:bool -> tree_contents list -> (Format.formatter -> unit) list
  end)
