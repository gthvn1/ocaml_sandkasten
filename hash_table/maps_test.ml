open OUnit2
open Maps

let map1 = HashMap.create (fun x -> x) 10

let assoc_tests =
  let open HashMap in
  [
    (let () = insert 1 "un" map1 in
     "find key 1" >:: fun _ -> assert_equal (Some "un") (find 1 map1));
    (let () = insert 2 "deux" map1 in
     "find key 2" >:: fun _ -> assert_equal (Some "deux") (find 2 map1));
    (let () = insert 2 "new" map1 in
     "replace key 2" >:: fun _ -> assert_equal (Some "new") (find 2 map1));
    ("don't find key 3" >:: fun _ -> assert_equal None (find 3 map1));
  ]

let suite = "map suite" >::: assoc_tests
let _ = run_test_tt_main suite
