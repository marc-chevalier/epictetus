open OUnit2

open Epictetus.Tabulator

let _test_print : test =
  let open StringTabulator in
  let test_cases : (string * tree_contents * tree_size * string) list = [
    "1", Leaf "", SLeaf 0, "";
    "2", Node [], SNode ([], 0), "";
    "3", Node [Leaf ""; Leaf ""], SNode ([SLeaf 0; SLeaf 0], 0), "";
    "4", Node [Leaf "ab"; Leaf "cde"], SNode ([SLeaf 2; SLeaf 3], 5), "abcde";
    "5", Node [Leaf "ab"; Leaf "cde"], SNode ([SLeaf 4; SLeaf 6], 12), "ab  cde     ";
    "6", Leaf "bla", SNode ([SLeaf 1; SLeaf 2], 4), "bla ";
    "7", Leaf "bla", SNode ([SLeaf 1; SLeaf 2], 3), "bla";
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
