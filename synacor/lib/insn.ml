type t =
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
type chunk = int * int option * int option * int option

(** [decode_set chunk] decodes the SET instruction. It uses 2 parameters
    of the chunk. It returns Set addr value and the
    size of the instruction is 3. *)
let decode_set (c : chunk) : (t * int) option =
  match c with
  | _, None, _, _ | _, _, None, _ ->
      None
  | _, Some addr, Some value, _ ->
      Some (Set (addr, value), 3)

(** [decode_jmp chunk] decodes the JMP instruction. It uses 1 parameter
    of the chunk that is the address. So it returns Jmp addr and the
    size of the instruction is 2. *)
let decode_jmp (c : chunk) : (t * int) option =
  match c with _, None, _, _ -> None | _, Some addr, _, _ -> Some (Jmp addr, 2)

(** [decode_jt chunk] decodes the JT instruction. It uses 2 parameters
    of the chunk that is the condition of the jump and the address.
    Returns Jt val addr and the size of the instruction is 3. *)
let decode_jt (c : chunk) : (t * int) option =
  match c with
  | _, None, _, _ | _, _, None, _ ->
      None
  | _, Some value, Some addr, _ ->
      Some (Jt (value, addr), 3)

(** [decode_jt chunk] decodes the JTinstruction. It uses 2 parameters
    of the chunk that is the condition of the jump and the address.
    Returns Jt val addr and the size of the instruction is 3. *)
let decode_jf (c : chunk) : (t * int) option =
  match c with
  | _, None, _, _ | _, _, None, _ ->
      None
  | _, Some value, Some addr, _ ->
      Some (Jf (value, addr), 3)

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
  | 0x0 ->
      Some (Halt, 1)
  | 0x1 ->
      decode_set c
  | 0x6 ->
      decode_jmp c
  | 0x7 ->
      decode_jt c
  | 0x8 ->
      decode_jf c
  | 0x13 ->
      decode_out c
  | 0x15 ->
      Some (Noop, 1)
  | _ ->
      failwith (Printf.sprintf "Unknown instruction 0x%x" c1)

let to_string (insn : t) : string =
  match insn with
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
