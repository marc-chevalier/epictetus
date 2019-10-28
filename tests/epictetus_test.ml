open OUnit2

open Epictetus_test_tree_size
open Epictetus_test_print

let run_tests () =
  let suite =
    "Epictetus" >::: [
      test_tree_size;
      test_print;
    ]
  in
  run_test_tt_main suite

let _ = run_tests ()

