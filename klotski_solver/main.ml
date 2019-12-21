(*
 * Compile:
 *   ocamlopt -c klotski.ml
 *   ocamlopt -c main.ml
 *   ocamlopt -o solver klotski.cmo main.cmo
 *
 * or if you want to run the tests:
 *   dune runtest
 *)

let onePiece = Klotski.S

let () = match onePiece with
  | S -> print_endline "square"
  | H -> print_endline "horizontal rectangle"
  | V -> print_endline "vertical rectangle"
  | C -> print_endline "small square"
  | X -> print_endline "nothing"
