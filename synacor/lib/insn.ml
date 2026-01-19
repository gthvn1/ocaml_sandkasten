type t = Halt | Out of char | Noop | Unknown
type chunk = int * int option * int option * int option

let decode_out (c : chunk) : (t * int) option =
  match c with
  | _, None, _, _ -> None
  | _, Some v, _, _ -> Some (Out (Char.chr v), 2)

let decode (c : chunk) : (t * int) option =
  let c1, _, _, _ = c in
  match c1 with
  | 0 -> Some (Halt, 0)
  | 0x13 -> decode_out c
  | 0x15 -> Some (Noop, 1)
  | _ -> Some (Unknown, 0)

let to_string (insn : t) : string =
  match insn with
  | Halt -> "HALT"
  | Out _ -> "OUT"
  | Noop -> "NOOP"
  | Unknown -> "UNKNOWN"
