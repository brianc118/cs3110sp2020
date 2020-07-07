open OUnit2
open Exercises

let product_tests =
  "test suite for product"
  >::: [
         ("empty" >:: fun _ -> assert_equal 1 (product []));
         ("two" >:: fun _ -> assert_equal 2 (product [ 2 ]));
         ("twothree" >:: fun _ -> assert_equal 6 (product [ 2; 3 ]));
       ]

let sorted_descending_tests =
  "test suite for sorted_descending"
  >::: [
         ( "easy" >:: fun _ ->
           assert_equal [ 4; 3; 2; 1 ] (sorted_descending [ 3; 2; 4; 1 ]) );
       ]

let list_max_tests =
  "test suite for list_max"
  >::: [
         ( "empty" >:: fun _ ->
           assert_raises (Failure "list_max") (fun () -> list_max []) );
         ("common" >:: fun _ -> assert_equal 4 (list_max [ 3; 2; 4; 1 ]));
       ]

let _ = run_test_tt_main product_tests

let _ = run_test_tt_main sorted_descending_tests

let _ = run_test_tt_main list_max_tests
