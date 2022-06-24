open OUnit2

open Epictetus

open Epictetus_test_lib

let _test_print : test =
  let open Std_aligners.StringAligner in
  let test_cases : (string * tree_contents * Size_tree.t * bool * string) list = [
    "t1", leaf "", sleaf 0, false, "";
    "t2", node [], snode [] 0, false, "";
    "t3", node [leaf ""; leaf ""], snode [sleaf 0; sleaf 0] 0, false, "";
    "t4", node [leaf "ab"; leaf "cde"], snode [sleaf 2; sleaf 3] 5, false, "abcde";
    "t5", node [leaf "ab"; leaf "cde"], snode [sleaf 4; sleaf 6] 12, false, "ab  cde";
    "t5*", node [leaf "ab"; leaf "cde"], snode [sleaf 4; sleaf 6] 12, true, "ab  cde     ";
    "t6", node ~fill_with:'*' [leaf ~fill_with:'-' "ab"; leaf ~fill_with:'/' "cde"], snode [sleaf 4; sleaf 6] 12, false, "ab--cde";
    "t6*", node ~fill_with:'*' [leaf ~fill_with:'-' "ab"; leaf ~fill_with:'/' "cde"], snode [sleaf 4; sleaf 6] 12, true, "ab--cde///**";
    "t7", node ~fill_with:'*' [leaf ~fill_with:'-' ~align:Right "ab"; leaf ~fill_with:'/' "cde"], snode [sleaf 4; sleaf 6] 12, false, "--abcde";
    "t7*", node ~fill_with:'*' [leaf ~fill_with:'-' ~align:Right "ab"; leaf ~fill_with:'/' "cde"], snode [sleaf 4; sleaf 6] 12, true, "--abcde///**";
    "t8", node ~fill_with:'*' [leaf ~fill_with:'-' "ab"; leaf ~fill_with:'/' ~align:Right "cde"], snode [sleaf 4; sleaf 6] 12, false, "ab--///cde";
    "t8*", node ~fill_with:'*' [leaf ~fill_with:'-' "ab"; leaf ~fill_with:'/' ~align:Right "cde"], snode [sleaf 4; sleaf 6] 12, true, "ab--///cde**";
    "t9", node ~fill_with:'*' ~align:Right [leaf ~fill_with:'-' "ab"; leaf ~fill_with:'/' "cde"], snode [sleaf 4; sleaf 6] 12, false, "**ab--cde";
    "t9*", node ~fill_with:'*' ~align:Right [leaf ~fill_with:'-' "ab"; leaf ~fill_with:'/' "cde"], snode [sleaf 4; sleaf 6] 12, true, "**ab--cde///";
    "t10", node ~fill_with:'*' ~align:Right [leaf ~fill_with:'-' ~align:Right "ab"; leaf ~fill_with:'/' ~align:Right "cde"], snode [sleaf 4; sleaf 6] 12, false, "**--ab///cde";
    "t11", node ~fill_with:'*' [leaf ~fill_with:'-' ~align:Center "ab"; leaf ~fill_with:'/' ~align:Center "cde"], snode [sleaf 4; sleaf 6] 12, false, "-ab-/cde";
    "t11*", node ~fill_with:'*' [leaf ~fill_with:'-' ~align:Center "ab"; leaf ~fill_with:'/' ~align:Center "cde"], snode [sleaf 4; sleaf 6] 12, true, "-ab-/cde//**";
    "t12", leaf ~fill_with:'-' "", snode [sleaf 4; sleaf 6] 12, false, "";
    "t12*", leaf ~fill_with:'-' "", snode [sleaf 4; sleaf 6] 12, true, "------------";
    "t13", leaf "bla", snode [sleaf 1; sleaf 2] 4, false, "bla";
    "t13*", leaf "bla", snode [sleaf 1; sleaf 2] 4, true, "bla ";
    "t14", leaf "bla", snode [sleaf 1; sleaf 2] 3, false, "bla";
  ]
  in
  let test (name, c, s, trailing_whitespaces, standard : string * tree_contents * Size_tree.t * bool * string) : test =
    let case (_test_ctxt: OUnitTest.ctxt) =
      assert_equal
        ~printer:(fun s -> Format.asprintf "<%s>" s)
        ~pp_diff:(fun fmt (a, b) -> Format.fprintf fmt "<%s> != <%s>" a b)
        standard
        (Format.asprintf "%a" (print_tree_with_size ~trailing_whitespaces s) c)
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
