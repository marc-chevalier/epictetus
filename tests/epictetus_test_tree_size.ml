open OUnit2

open Epictetus

open Epictetus_test_lib

let _test_tree_size : test =
  let open Std_aligners.StringAligner in
  let test_cases : (string * tree_contents * Size_tree.t) list = [
    "empty_leaf", leaf "", sleaf 0;
    "trivial", node [], snode [] 0;
    "2_empty_leaves", node [leaf ""; leaf ""], snode [sleaf 0; sleaf 0] 0;
    "2_leaves", node [leaf "ab"; leaf "cde"], snode [sleaf 2; sleaf 3] 5;
  ]
  in
  let test (name, f, standard : string * tree_contents * Size_tree.t) : test =
    let case (_test_ctxt: OUnitTest.ctxt) =
      assert_equal
        ~printer:(fun s -> Format.asprintf "%a" pp_string_tree_size s)
        ~pp_diff:(fun fmt (a, b) -> Format.fprintf fmt "%a != %a" pp_string_tree_size a pp_string_tree_size b)
        (tree_size f)
        standard
    in
    name >:: case
  in
  let test_shape (name, f, _standard : string * tree_contents * Size_tree.t) : test =
    let case (_test_ctxt: OUnitTest.ctxt) =
      assert_bool
        (name^"_shape")
        (has_same_shape f (tree_size f))
    in
    name >:: case
  in
  let test_consistent (name, f, _standard : string * tree_contents * Size_tree.t) : test =
    let case (_test_ctxt: OUnitTest.ctxt) =
      assert_bool
        (name^"_consistent")
        (f |> tree_size |> consistent_tree_size)
    in
    name >:: case
  in
  "" >::: [
    "value" >::: List.map test test_cases;
    "shape" >::: List.map test_shape test_cases;
    "consistent" >::: List.map test_consistent test_cases;
  ]

let _test_merge_size_trees : test =
  let test_cases : (string * Size_tree.t * Size_tree.t * Size_tree.t) list = [
    "empty_leaves", sleaf 0, sleaf 0, sleaf 0;
    "empty_node_empty_leaf", snode [] 0, sleaf 0, snode [] 0;
    "non_empty_node_empty_leaf", snode [] 10, sleaf 0, snode [] 10;
    "empty_node_non_empty_leaf", snode [] 0, sleaf 10, snode [] 10;
    "2_empty_leaves_empty_leaf", snode [sleaf 0; sleaf 0] 0, sleaf 0, snode [sleaf 0; sleaf 0] 0;
    "non_empty_node_non_empty_leaf", snode [sleaf 2; sleaf 3] 5, sleaf 10, snode [sleaf 2; sleaf 3] 10;
    "inconsistent_nodes", snode [sleaf 2; sleaf 3] 5, snode [sleaf 5; sleaf 1; sleaf 8] 14, snode [sleaf 5; sleaf 3; sleaf 8] 16;
  ]
  in
  let test (name, a, b, standard : string * Size_tree.t * Size_tree.t * Size_tree.t) : test =
    let case (_test_ctxt: OUnitTest.ctxt) =
      assert_equal
        ~printer:(fun s -> Format.asprintf "%a" pp_string_tree_size s)
        ~pp_diff:(fun fmt (a, b) -> Format.fprintf fmt "%a != %a" pp_string_tree_size a pp_string_tree_size b)
        (Size_tree.merge a b)
        standard
    in
    name >:: case
  in
  let test_subseteq (name, a, b, _standard : string * Size_tree.t * Size_tree.t * Size_tree.t) : test =
    let case1 (_test_ctxt: OUnitTest.ctxt) =
      assert_bool
        (name^"_subseteq1")
        (subseteq a (Size_tree.merge a b))
    in
    let case2 (_test_ctxt: OUnitTest.ctxt) =
      assert_bool
        (name^"_subseteq2")
        (subseteq a (Size_tree.merge a b))
    in
    name >::: [
      "left" >:: case1;
      "right" >:: case2;
    ]
  in
  let test_consistent (name, a, b, _standard : string * Size_tree.t * Size_tree.t * Size_tree.t) : test =
    let case (_test_ctxt: OUnitTest.ctxt) =
      assert_bool
        (name^"_consistent")
        (Size_tree.merge a b |> consistent_tree_size)
    in
    name >:: case
  in
  "" >::: [
    "value" >::: List.map test test_cases;
    "shape" >::: List.map test_subseteq test_cases;
    "consistent" >::: List.map test_consistent test_cases;
  ]

let test_tree_size : test =
  let suite =
    "tree_size" >::: [
      _test_tree_size;
      _test_merge_size_trees;
    ]
  in
  suite
