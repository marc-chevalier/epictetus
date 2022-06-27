# Epictetus
(Elegant Printer of Insanely Complex Tables Expressing Trees with Uneven Shapes)

Epictetus print tables with (sub-)\*columns so that corresponding columns (if any) are aligned.

## The Big Picture

The simplest use-case is to align columns of a table. For instance if one has the lines (with pipes only to separate columns):
```
a |foo |b
bla |loooooong |c
```
Epictetus will print
```
a   foo       b
bla loooooong c
```

It also works with lines of unequal length. For instance, for the lines
```
a |foo
bla |loooooong |c |d
```
Epictetus will print
```
a   foo
bla loooooong c d
```

But this is a bit limited. What Epictetus is good at is to align tables with columns and sub-columns, aritrarily nested. Even when each line has a different subdivision.

For instance, with parentheses to signify nesting:
```
(a |b |c) |d
foo |bar
```
will be printed as
```
a b c d
foo   bar
```
Or:
```
(a |b |c) |d
longwordlargecolumn |bar
```
will be printed as
```
a b c               d
longwordlargecolumn bar
```
Of course, with more lines:
```
(a |b |c) |d
longwordlargecolumn |bar
(bla |bli) |e
```
```
a   b   c           d
longwordlargecolumn bar
bla bli             e
```

## How To Use

Each line is defined as a tree whose leaves contain the value to print. each node (internal and leaves) have 2 extra attributes:
- the character to fill blank space with;
- the alignment to write the text with, when the column is wider than the text to print (can be left, right or centered).

The type of the content is parametric. The signature `ALIGNER` gives the interface of such printing module. It is implemented as a functor by `GenericAligner`. The functor parameter has signature
```ocaml
module type CONTENTS =
  (sig
    type contents
    val contents_length: contents -> int
    val pp: Format.formatter -> contents -> unit
  end)
```
It gives the type of the content, how to compute the length of such a content and how to print it. Three standard instanciations are given in `Std_aligners`.

When the content is a pretty printer, with explicit size:
```ocaml
module Sized_pp_contents
  : Aligner_sig.CONTENTS
    with type contents = (Format.formatter -> unit) * int
module SizedPPAligner = Generic_aligner.GenericAligner(Sized_pp_contents)
```

When the content is a pretty printer, the size being computed by using the pretty printer:
```ocaml
module Pp_contents
  : Aligner_sig.CONTENTS
    with type contents = Format.formatter -> unit
module PPAligner = Generic_aligner.GenericAligner(Pp_contents)
```

When the content are simply strings:
```ocaml
module String_contents
  : Aligner_sig.CONTENTS
    with type contents = string
module StringAligner = Generic_aligner.GenericAligner(String_contents)
```

To print, assuming using the `StringAligner`, the easiest way
```ocaml
let table =
  StringAligner.[
    node [
      node [
        leaf "a";
        leaf "b";
        leaf "c";
      ];
      leaf "d";
    ];
    node [
      leaf "longwordlargecolumn";
      leaf "bar";
    ];
    node [
      node [
        leaf "bla";
        leaf "bli";
      ];
      leaf "e";
    ];
  ]
in
let () = Format.printf "%a" StringAligner.print_table table in
...
```

For all other printing functions and options, please refer to `src/aligner_sig.mli` and `src/generic_aligner.mli`.
