open OUnit2
module B = Ocaml_exercises_lib.Beginner

let test_last_two_two_ints _ = assert_equal (Some (1, 4)) (B.last_two [ 1; 4 ])

let test_last_two_strings _ =
  assert_equal (Some ("c", "d")) (B.last_two [ "a"; "b"; "c"; "d" ])

let test_last_two_singleton _ = assert_equal None (B.last_two [ 1 ])
let test_last_two_empty _ = assert_equal None (B.last_two [])

let () =
  run_test_tt_main
    ("Beginner tests"
    >::: [
           "last_two with two ints" >:: test_last_two_two_ints;
           "last_two strings" >:: test_last_two_strings;
           "last_two singleton" >:: test_last_two_singleton;
           "last_two empty" >:: test_last_two_empty;
         ])
