(*
 * ocamlc -o aoc aoc.ml
 *)
let read_file filename =
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true; do
      lines := int_of_string (input_line chan) :: !lines
    done;
    List.rev !lines
  with _ ->
    close_in_noerr chan;
    List.rev !lines

(* Day 1 *)
let fuelNeeded mass = mass / 3 - 2

(* Return the totalFuel as a string *)
let totalFuel fileName =
  let l = read_file fileName in
  string_of_int (List.fold_left (fun acc x -> acc + (fuelNeeded x)) 0 l)
