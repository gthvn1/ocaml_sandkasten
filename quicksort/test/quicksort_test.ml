open Printf
open Quicksort

let%expect_test _ =
  let l = quicksort [2; 3; 1; 2] in
  List.iter (printf "%d ") l;
  [%expect{| 1 2 2 3 |}]

