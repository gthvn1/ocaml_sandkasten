type t =
  | Add of (int * int * int)
  | Eq of (int * int * int)
  | Halt
  | Jmp of int
  | Jt of (int * int)
  | Jf of (int * int)
  | Noop
  | Out of char
  | Set of (int * int)
  | Unknown

(** This is the memory area that contains the maximum number of words
    required by an instruction. The first integer is the opcode and it
    is required. Others are optional and their usage depends on the kind
    of the instruction. *)
type chunk = {opcode: int; ops: int option list}

let take_ops n (ops : int option list) : int list option =
  let rec aux lst acc i =
    if i = n then Some (List.rev acc)
    else
      match lst with
      | [] ->
          None
      | None :: _ ->
          None
      | Some v :: xs ->
          aux xs (v :: acc) (i + 1)
  in
  aux ops [] 0

(** [decode chunk] decodes the instruction based on the first element of the chunk that
    is the opcode. It returns the instruction and its size in memory to be able to
    update the instruction pointer when executing. *)
let decode (c : chunk) : (t * int) option =
  match c.opcode with
  | 0 ->
      (* halt: 0 => stop execution and terminate the program *)
      Some (Halt, 1)
  | 1 -> (
      (* set: 1 a b => set register <a> to the value of <b> *)
      let ops_size = 2 in
      match take_ops ops_size c.ops with
      | Some [a; b] ->
          Some (Set (a, b), ops_size + 1)
      | _ ->
          None )
  | 4 -> (
      (* eq: 4 a b c => set <a> to 1 if <b> is equal to <c>; set it to 0 otherwise *)
      let ops_size = 3 in
      match take_ops ops_size c.ops with
      | Some [a; b; c] ->
          Some (Eq (a, b, c), ops_size + 1)
      | _ ->
          None )
  | 6 -> (
      (* jmp: 6 a => jump to <a> *)
      let ops_size = 1 in
      match take_ops ops_size c.ops with
      | Some [a] ->
          Some (Jmp a, ops_size + 1)
      | _ ->
          None )
  | 7 -> (
      (* jt: 7 a b *)
      (* if <a> is nonzero, jump to <b> *)
      let ops_size = 2 in
      match take_ops ops_size c.ops with
      | Some [a; b] ->
          Some (Jt (a, b), ops_size + 1)
      | _ ->
          None )
  | 8 -> (
      (* jf: 8 a b *)
      (* if <a> is zero, jump to <b> *)
      let ops_size = 2 in
      match take_ops ops_size c.ops with
      | Some [a; b] ->
          Some (Jf (a, b), ops_size + 1)
      | _ ->
          None )
  | 9 -> (
      (* add: 9 a b c *)
      (* assign into <a> the sum of <b> and <c> (modulo 32768) *)
      let ops_size = 3 in
      match take_ops ops_size c.ops with
      | Some [a; b; c] ->
          Some (Add (a, b, c), ops_size + 1)
      | _ ->
          None )
  | 19 -> (
      (* out: 19 a *)
      (* write the character represented by ascii code <a> to the terminal *)
      let ops_size = 1 in
      match take_ops ops_size c.ops with
      | Some [a] ->
          Some (Out (Char.chr a), ops_size + 1)
      | _ ->
          None )
  | 21 ->
      (* noop: 21 => no operation *)
      Some (Noop, 1)
  | _ ->
      failwith (Printf.sprintf "Unknown instruction 0x%x" c.opcode)

let to_string (insn : t) : string =
  match insn with
  | Add (a, b, c) ->
      Printf.sprintf "ADD 0x%x 0x%x 0x%x" a b c
  | Eq (a, b, c) ->
      Printf.sprintf "EQ 0x%x 0x%x 0x%x" a b c
  | Halt ->
      "HALT"
  | Jmp addr ->
      Printf.sprintf "JUMP 0x%x" addr
  | Jt (value, addr) ->
      Printf.sprintf "JT %d 0x%x" value addr
  | Jf (value, addr) ->
      Printf.sprintf "JF %d 0x%x" value addr
  | Noop ->
      "NOOP"
  | Out c ->
      Printf.sprintf "OUT %c" c
  | Set (reg, addr) ->
      Printf.sprintf "SET %d 0x%x" reg addr
  | Unknown ->
      "UNKNOWN"
