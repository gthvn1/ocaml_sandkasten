open OUnit2
open Maps

let binding_test name input output =
  name >:: fun _ -> assert_equal input (AssocListMap.bindings output)

let l1 = [ (3110, "fun") ]

let assoc_tests =
  let open AssocListMap in
  [
    binding_test "empty has no bindings" [] empty;
    binding_test "singleton has 1 binding" l1 (of_list l1);
  ]

let suite = "map suite" >::: assoc_tests
let _ = run_test_tt_main suite
