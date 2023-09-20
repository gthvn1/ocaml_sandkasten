open OUnit2

let tests = "just a test" >::: [ ("empty" >:: fun _ -> assert_equal 0 0) ]
let _ = run_test_tt_main tests
