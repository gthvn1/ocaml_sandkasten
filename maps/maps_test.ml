open OUnit2
open Maps

let assoc_tests =
  let open AssocListMap in
  [ ("empty has no bindings" >:: fun _ -> assert_equal [] (bindings empty)) ]

let suite = "map suite" >::: assoc_tests
let _ = run_test_tt_main suite
