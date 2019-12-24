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
  with e ->
    close_in_noerr chan;
    List.rev !lines

(* Day 1 *)
let fuelNeeded mass = mass / 3 - 2

(* Return the totalFuel as a string *)
let totalFuel fileName =
  let l = read_file fileName in
  string_of_int (List.fold_left (fun acc x -> acc + (fuelNeeded x)) 0 l)

(* Day 2 *)
let prog1 = [| 1; 0; 0; 3; 99 |];;
let prog2 = [| 1; 3; 0; 3; 99 |];;
let prog3 = [| 1; 0; 0; 3; 1; 1; 2; 3; 99 |];;
let prog4 = [| 1;1;1;4;99;5;6;0;99 |];;
let prog5 =
  [| 1;12;2;3; 1;1;2;3; 1;3;4;3; 1;5;0;3; 2;1;10;19; 2;9;19;23;
     2;23;10;27; 1;6;27;31; 1;31;6;35; 2;35;10;39; 1;39;5;43;
     2;6;43;47; 2;47;10;51; 1;51;6;55; 1;55;6;59; 1;9;59;63;
     1;63;9;67; 1;67;6;71; 2;71;13;75; 1;75;5;79; 1;79;9;83;
     2;6;83;87; 1;87;5;91; 2;6;91;95; 1;95;9;99; 2;6;99;103;
     1;5;103;107; 1;6;107;111; 1;111;10;115; 2;115;13;119;
     1;119;6;123; 1;123;2;127; 1;127;5;0; 99; 2;14;0;0 |]

exception UnkownOpcode

type opcode = Add | Sub | Halt

let int_to_opcode = function
  | 1 -> Add
  | 2 -> Sub
  | 99 -> Halt
  | op -> (print_int op; print_newline (); raise UnkownOpcode)

let get_insn code pos =
  let opcode = int_to_opcode(Array.get code (pos + 0)) in
  match opcode with
  | Halt -> (Halt, 0, 0, 0)
  | _ -> (opcode, Array.get code (pos + 1), Array.get code (pos + 2), Array.get code (pos + 3))

let next_insn pos = pos + 4

(*
 * We don't want to modify the code during the execution. So make a copy and do the
 * execution on the copy
 *)
let execute code =
  let insns = Array.copy code in
  let rec run_insns pos =
    let (op, a, b, c) = get_insn insns pos in
    match op with
    | Halt -> insns
    | Add -> ( Array.set insns c (insns.(a) + insns.(b)); run_insns (next_insn pos) )
    | Sub -> ( Array.set insns c (insns.(a) * insns.(b)); run_insns (next_insn pos) )
  in
  run_insns 0

let () =
  let executed_code = execute prog5 in
  print_string "Day1 > ";
  print_endline (totalFuel "input_day1.data");
  print_string "Day2 > ";
  print_int executed_code.(0);
  print_newline ()
