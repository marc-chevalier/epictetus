open OUnit2

open Epictetus.Tabulator

open Epictetus_test_lib

let _test_print : test =
  let open StringTabulator in
  let test_cases : (string * tree_contents * tree_size * string) list = [
    "1", leaf "", sleaf 0, "";
    "2", node [], snode [] 0, "";
    "3", node [leaf ""; leaf ""], snode [sleaf 0; sleaf 0] 0, "";
    "4", node [leaf "ab"; leaf "cde"], snode [sleaf 2; sleaf 3] 5, "abcde";
    "5", node [leaf "ab"; leaf "cde"], snode [sleaf 4; sleaf 6] 12, "ab  cde     ";
    "5", node ~fill_with:'*' [leaf ~fill_with:'-' "ab"; leaf ~fill_with:'/' "cde"], snode [sleaf 4; sleaf 6] 12, "ab--cde///**";
    "5", node ~fill_with:'*' [leaf ~fill_with:'-' ~align:Right "ab"; leaf ~fill_with:'/' "cde"], snode [sleaf 4; sleaf 6] 12, "--abcde///**";
    "5", node ~fill_with:'*' [leaf ~fill_with:'-' "ab"; leaf ~fill_with:'/' ~align:Right "cde"], snode [sleaf 4; sleaf 6] 12, "ab--///cde**";
    "5", node ~fill_with:'*' ~align:Right [leaf ~fill_with:'-' "ab"; leaf ~fill_with:'/' "cde"], snode [sleaf 4; sleaf 6] 12, "**ab--cde///";
    "5", node ~fill_with:'*' ~align:Right [leaf ~fill_with:'-' ~align:Right "ab"; leaf ~fill_with:'/' ~align:Right "cde"], snode [sleaf 4; sleaf 6] 12, "**--ab///cde";
    "5", node ~fill_with:'*' [leaf ~fill_with:'-' ~align:Center "ab"; leaf ~fill_with:'/' ~align:Center "cde"], snode [sleaf 4; sleaf 6] 12, "-ab-/cde//**";
    "5", leaf ~fill_with:'-' "", snode [sleaf 4; sleaf 6] 12,  "------------";
    "6", leaf "bla", snode [sleaf 1; sleaf 2] 4, "bla ";
    "7", leaf "bla", snode [sleaf 1; sleaf 2] 3, "bla";
  ]
  in
  let test (name, c, s, standard : string * tree_contents * tree_size * string) : test =
    let case (_test_ctxt: OUnitTest.ctxt) =
      assert_equal
        ~printer:(fun s -> Format.asprintf "<%s>" s)
        ~pp_diff:(fun fmt (a, b) -> Format.fprintf fmt "<%s> != <%s>" a b)
        (Format.asprintf "%a" (print_tree_with_size s) c)
        standard
    in
    name >:: case
  in
  "" >::: [
    "value" >::: List.map test test_cases;
  ]

let test_print : test =
  let suite =
    "print" >::: [
      _test_print;
    ]
  in
  suite
