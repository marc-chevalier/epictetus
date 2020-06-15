open OUnit2

open Epictetus

open Epictetus_test_lib

let _test_print : test =
  let open Std_aligners.StringAligner in
  let test_cases : (string * tree_contents * Size_tree.t * string) list = [
    "1", leaf "", sleaf 0, "";
    "2", node [], snode [] 0, "";
    "3", node [leaf ""; leaf ""], snode [sleaf 0; sleaf 0] 0, "";
    "4", node [leaf "ab"; leaf "cde"], snode [sleaf 2; sleaf 3] 5, "abcde";
    "5", node [leaf "ab"; leaf "cde"], snode [sleaf 4; sleaf 6] 12, "ab  cde     ";
    "6", node ~fill_with:'*' [leaf ~fill_with:'-' "ab"; leaf ~fill_with:'/' "cde"], snode [sleaf 4; sleaf 6] 12, "ab--cde///**";
    "7", node ~fill_with:'*' [leaf ~fill_with:'-' ~align:Right "ab"; leaf ~fill_with:'/' "cde"], snode [sleaf 4; sleaf 6] 12, "--abcde///**";
    "8", node ~fill_with:'*' [leaf ~fill_with:'-' "ab"; leaf ~fill_with:'/' ~align:Right "cde"], snode [sleaf 4; sleaf 6] 12, "ab--///cde**";
    "9", node ~fill_with:'*' ~align:Right [leaf ~fill_with:'-' "ab"; leaf ~fill_with:'/' "cde"], snode [sleaf 4; sleaf 6] 12, "**ab--cde///";
    "10", node ~fill_with:'*' ~align:Right [leaf ~fill_with:'-' ~align:Right "ab"; leaf ~fill_with:'/' ~align:Right "cde"], snode [sleaf 4; sleaf 6] 12, "**--ab///cde";
    "11", node ~fill_with:'*' [leaf ~fill_with:'-' ~align:Center "ab"; leaf ~fill_with:'/' ~align:Center "cde"], snode [sleaf 4; sleaf 6] 12, "-ab-/cde//**";
    "12", leaf ~fill_with:'-' "", snode [sleaf 4; sleaf 6] 12,  "------------";
    "13", leaf "bla", snode [sleaf 1; sleaf 2] 4, "bla ";
    "14", leaf "bla", snode [sleaf 1; sleaf 2] 3, "bla";
  ]
  in
  let test (name, c, s, standard : string * tree_contents * Size_tree.t * string) : test =
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
