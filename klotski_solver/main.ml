(*
 * Compile:
 *   ocamlopt -c klotski.ml
 *   ocamlopt -c main.ml
 *   ocamlopt -o solver klotski.cmo main.cmo
 *
 * or:
 *   dune build main.exe
 *)

(* List of Tests *)
let test_near0 () =
  Alcotest.(check (list int)) "same lists" [-2; -1; 0; 1; 2] (Klotski.near 0)

let test_near2 () =
  Alcotest.(check (list int)) "same lists" [0; 1; 2; 3; 4] (Klotski.near 2)

(* Run tests *)
let () =
  let open Alcotest in
  run "Utils" [
    "near", [
      test_case "near 0" `Quick test_near0;
      test_case "near 2" `Quick test_near2;
    ];
  ]
