open OUnit2
open Exercises

let is_valid_matrix_tests =
  "test suite for is_valid_matrix"
  >::: [
         ( "valid" >:: fun _ ->
           assert_equal true (is_valid_matrix [ [ 1; 2 ]; [ 3; 4 ] ]) );
         ( "invalid" >:: fun _ ->
           assert_equal false (is_valid_matrix [ [ 1; 2 ]; [ 3 ] ]) );
         ("invalid_empty" >:: fun _ -> assert_equal false (is_valid_matrix []));
         ( "invalid_empty2" >:: fun _ ->
           assert_equal false (is_valid_matrix [ []; [] ]) );
       ]

let add_row_vectors_test =
  "test suite for add_row_vectors"
  >::: [
         ("empty" >:: fun _ -> assert_equal [] (add_row_vectors [] []));
         ( "normal" >:: fun _ ->
           assert_equal [ 4; 6 ] (add_row_vectors [ 1; 2 ] [ 3; 4 ]) );
       ]

let add_matrices_test =
  "test suite for add_matrices"
  >::: [
         ( "normal" >:: fun _ ->
           assert_equal [ [ 6; 8 ]; [ 10; 12 ] ]
             (add_matrices [ [ 1; 2 ]; [ 3; 4 ] ] [ [ 5; 6 ]; [ 7; 8 ] ]) );
       ]

let multiply_matrices_test =
  "test suite for multiply_matrices"
  >::: [
        ( "normal" >:: fun _ ->
          assert_equal
            [ [ 30; 36; 42 ]; [ 66; 81; 96 ] ]
            (multiply_matrices
               [ [ 1; 2; 3 ]; [ 4; 5; 6 ] ]
               [ [ 1; 2; 3 ]; [ 4; 5; 6 ]; [ 7; 8; 9 ] ]) );
      ]

let _ = run_test_tt_main is_valid_matrix_tests

let _ = run_test_tt_main add_row_vectors_test

let _ = run_test_tt_main add_matrices_test

let _ = run_test_tt_main multiply_matrices_test
