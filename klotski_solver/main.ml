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
  | Klotski.S -> print_endline "It is a square"
  | Klotski.H -> print_endline "It is an horizontal rectangle"
  | Klotski.V -> print_endline "It is a vertical rectangle"
  | Klotski.C -> print_endline "It s a little square"
  | Klotski.X -> print_endline "It is an empty space"
