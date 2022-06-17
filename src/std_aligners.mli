[@@@warning "+A"]

(** A default ALIGNER in which contents are pretty-printers with the expected
    output size. Especially useful when using semantic tags or when printing
    ANSI escape sequences.
*)
module SizedPPAligner : Aligner_sig.ALIGNER with type contents = (Format.formatter -> unit) * int

(** A default ALIGNER in which contents are pretty-printers with the expected
    output size. Especially useful when using semantic tags or when printing
    ANSI escape sequences. The size is computed by calling the pretty-printer.
    It would better be pure!
*)
module PPAligner : Aligner_sig.ALIGNER with type contents = Format.formatter -> unit

(** A default ALIGNER in which contents are string. Very straightforward.
*)
module StringAligner : Aligner_sig.ALIGNER with type contents = string
