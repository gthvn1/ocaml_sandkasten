type t = Halt | Jmp of int | Noop | Out of char | Unknown

type chunk = int * int option * int option * int option

let decode_jmp (c : chunk) : (t * int) option =
  match c with _, None, _, _ -> None | _, Some addr, _, _ -> Some (Jmp addr, 2)

let decode_out (c : chunk) : (t * int) option =
  match c with
  | _, None, _, _ ->
      None
  | _, Some v, _, _ ->
      Some (Out (Char.chr v), 2)

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
