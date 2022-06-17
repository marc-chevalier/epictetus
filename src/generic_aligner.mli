[@@@warning "+A"]

(** Build an ALIGNER given a CONTENTS module.

In
    val leaf: ?fill_with:char -> ?align:alignment -> contents -> tree_contents
    val node: ?fill_with:char -> ?align:alignment -> tree_contents list -> tree_contents
default values are
- fill_with: ' '
- align: Left
*)
module GenericAligner (T: Aligner_sig.CONTENTS) : Aligner_sig.ALIGNER with type contents = T.contents
