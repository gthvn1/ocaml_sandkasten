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
let fuel_needed mass = mass / 3 - 2

let real_fuel_needed mass =
  let rec aux acc mass =
    let fuel = fuel_needed mass in
    if fuel <= 0 then acc
    else aux (acc + fuel) fuel in
  aux 0 mass

(* Return the totalFuel as a string *)
let total_fuel file_name =
  let l = read_file file_name in
  string_of_int (List.fold_left (fun acc x -> acc + (fuel_needed x)) 0 l)

let real_total_fuel file_name =
  let l = read_file file_name in
  string_of_int (List.fold_left (fun acc x -> acc + (real_fuel_needed x)) 0 l)
