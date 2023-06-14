open OUnit2
open Maps

let binding_test name input output =
  name >:: fun _ -> assert_equal input (AssocListMap.bindings output)

let l1 = [ (3110, "fun") ]
let l2 = [ (3110, "fun"); (2110, "oo")]

let assoc_tests =
  let open AssocListMap in
  [
    binding_test "empty has no bindings" [] empty;
    binding_test "singleton has 1 binding" l1 (of_list l1);
    binding_test "list with 2 bindings" l2 (of_list l2);
    binding_test "adding 1 binding" l1 (empty |> insert 3110 "fun");
    binding_test "adding 2 bindings with same key" l1 (empty |> insert 3110 "nop" |> insert 3110 "fun")
  ]

let suite = "map suite" >::: assoc_tests
let _ = run_test_tt_main suite
