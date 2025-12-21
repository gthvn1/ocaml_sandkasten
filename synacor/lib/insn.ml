type t = Halt | Jmp of int | Noop | Out of char | Unknown

(** This is the memory area that contains the maximum number of words
    required by an instruction. The first integer is the opcode and it
    is required. Others are optional and their usage depends on the kind
    of the instruction. *)
type chunk = int * int option * int option * int option

(** [decode_jmp chunk] decodes the JMP instruction. It uses 1 parameter
    of the chunk that is the address. So it returns Jmp addr and the
    size of the instruction is 2. *)
let decode_jmp (c : chunk) : (t * int) option =
  match c with _, None, _, _ -> None | _, Some addr, _, _ -> Some (Jmp addr, 2)

(** [decode_out chunk] decodes the OUT instruction. It uses 1 parameter
    of the chunk that is the character to be printed. It returns OUT char
    and its size is 2. *)
let decode_out (c : chunk) : (t * int) option =
  match c with
  | _, None, _, _ ->
      None
  | _, Some v, _, _ ->
      Some (Out (Char.chr v), 2)

(** [decode chunk] decodes the instruction base on the first element of the chunk that
    is the opcode. It return the instruction and its size in memory to be able to
    update the instruction pointer when executing. The chunk is the piece of memory
    fetched. *)
let decode (c : chunk) : (t * int) option =
  let c1, _, _, _ = c in
  match c1 with
  | 0 ->
      Some (Halt, 0)
  | 0x6 ->
      decode_jmp c
  | 0x13 ->
      decode_out c
  | 0x15 ->
      Some (Noop, 1)
  | _ ->
      Some (Unknown, 0)

let to_string (insn : t) : string =
  match insn with
  | Halt ->
      "HALT"
  | Jmp _ ->
      "JUMP"
  | Noop ->
      "NOOP"
  | Out _ ->
      "OUT"
  | Unknown ->
      "UNKNOWN"
