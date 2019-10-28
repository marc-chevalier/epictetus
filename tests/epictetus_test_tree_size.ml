open OUnit2

open Epictetus.Tabulator

open Epictetus_test_lib

let _test_tree_size : test =
  let open StringTabulator in
  let test_cases : (string * tree_contents * tree_size) list = [
    "empty_leaf", Leaf "", SLeaf 0;
    "trivial", Node [], SNode ([], 0);
    "2_empty_leaves", Node [Leaf ""; Leaf ""], SNode ([SLeaf 0; SLeaf 0], 0);
    "2_leaves", Node [Leaf "ab"; Leaf "cde"], SNode ([SLeaf 2; SLeaf 3], 5);
  ]
  in
  let test (name, f, standard : string * tree_contents * tree_size) : test =
    let case (_test_ctxt: OUnitTest.ctxt) =
      assert_equal
        ~printer:(fun s -> Format.asprintf "%a" pp_string_tree_size s)
        ~pp_diff:(fun fmt (a, b) -> Format.fprintf fmt "%a != %a" pp_string_tree_size a pp_string_tree_size b)
        (tree_size f)
        standard
    in
    name >:: case
  in
  let test_shape (name, f, _standard : string * tree_contents * tree_size) : test =
    let case (_test_ctxt: OUnitTest.ctxt) =
      assert_bool
        (name^"_shape")
        (has_same_shape f (tree_size f))
    in
    name >:: case
  in
  let test_consistent (name, f, _standard : string * tree_contents * tree_size) : test =
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
  let test_cases : (string * tree_size * tree_size * tree_size) list = [
    "empty_leaves", SLeaf 0, SLeaf 0, SLeaf 0;
    "empty_node_empty_leaf", SNode ([], 0), SLeaf 0, SNode ([], 0);
    "non_empty_node_empty_leaf", SNode ([], 10), SLeaf 0, SNode ([], 10);
    "empty_node_non_empty_leaf", SNode ([], 0), SLeaf 10, SNode ([], 10);
    "2_empty_leaves_empty_leaf", SNode ([SLeaf 0; SLeaf 0], 0), SLeaf 0, SNode ([SLeaf 0; SLeaf 0], 0);
    "non_empty_node_non_empty_leaf", SNode ([SLeaf 2; SLeaf 3], 5), SLeaf 10, SNode ([SLeaf 2; SLeaf 3], 10);
    "inconsistent_nodes", SNode ([SLeaf 2; SLeaf 3], 5), SNode ([SLeaf 5; SLeaf 1; SLeaf 8], 14), SNode ([SLeaf 5; SLeaf 3; SLeaf 8], 16);
  ]
  in
  let test (name, a, b, standard : string * tree_size * tree_size * tree_size) : test =
    let case (_test_ctxt: OUnitTest.ctxt) =
      assert_equal
        ~printer:(fun s -> Format.asprintf "%a" pp_string_tree_size s)
        ~pp_diff:(fun fmt (a, b) -> Format.fprintf fmt "%a != %a" pp_string_tree_size a pp_string_tree_size b)
        (merge_size_trees a b)
        standard
    in
    name >:: case
  in
  let test_subseteq (name, a, b, _standard : string * tree_size * tree_size * tree_size) : test =
    let case1 (_test_ctxt: OUnitTest.ctxt) =
      assert_bool
        (name^"_subseteq1")
        (subseteq a (merge_size_trees a b))
    in
    let case2 (_test_ctxt: OUnitTest.ctxt) =
      assert_bool
        (name^"_subseteq2")
        (subseteq a (merge_size_trees a b))
    in
    name >::: [
      "left" >:: case1;
      "right" >:: case2;
    ]
  in
  let test_consistent (name, a, b, _standard : string * tree_size * tree_size * tree_size) : test =
    let case (_test_ctxt: OUnitTest.ctxt) =
      assert_bool
        (name^"_consistent")
        (merge_size_trees a b |> consistent_tree_size)
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
