[@@@warning "+A"]
module Sized_pp_content
  : Aligner.CONTENT
    with type contents = (Format.formatter -> unit) * int
  =
  (struct
    type contents = (Format.formatter -> unit) * int
    let contents_length : contents -> int = snd
    let pp (fmt: Format.formatter) (pp, _: contents) : unit = pp fmt
  end)

module SizedPPAligner = Generic_aligner.GenericAligner(Sized_pp_content)

module Pp_content
  : Aligner.CONTENT
    with type contents = Format.formatter -> unit
  =
  (struct
    type contents = Format.formatter -> unit
    let contents_length (c: contents) : int = Format.kasprintf String.length "%t" c
    let pp (fmt: Format.formatter) (pp: contents) : unit = pp fmt
  end)

module PPAligner = Generic_aligner.GenericAligner(Pp_content)

module String_content
  : Aligner.CONTENT
    with type contents = string
  =
  (struct
    type contents = string
    let contents_length : string -> int = String.length
    let pp (fmt: Format.formatter) (c : contents) : unit = Format.pp_print_string fmt c
  end)

module StringAligner = Generic_aligner.GenericAligner(String_content)
