(* Day 2 *)
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
let execute code noun verb =
  let insns = Array.copy code in
  let rec run_insns ip =
    let (op, a, b, c) = get_insn insns ip in
    match op with
    | Halt -> insns
    | Add -> ( Array.set insns c (insns.(a) + insns.(b)); run_insns (next_insn ip) )
    | Sub -> ( Array.set insns c (insns.(a) * insns.(b)); run_insns (next_insn ip) )
  in
  Array.set insns 1 noun;
  Array.set insns 2 verb;
  (* run the program with ip (instruction pointer) set to 0 *)
  run_insns 0
