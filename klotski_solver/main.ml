(*
 * Compile:
 *   ocamlopt -c klotski.ml
 *   ocamlopt -c main.ml
 *   ocamlopt -o solver klotski.cmo main.cmo
 *
 * or:
 *   dune build main.exe
 *)

let onePiece = Klotski.S

let () = match onePiece with
  | S -> print_endline "square"
  | H -> print_endline "horizontal rectangle"
  | V -> print_endline "vertical rectangle"
  | C -> print_endline "small square"
  | X -> print_endline "nothing"
