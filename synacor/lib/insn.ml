type t = Halt | Out | Noop | Unknown

let of_char (c : char) : t =
  match Char.code c with 0 -> Halt | 19 -> Out | 21 -> Noop | _ -> Unknown

let to_string (insn : t) : string =
  match insn with
  | Halt ->
      "HALT"
  | Out ->
      "OUT"
  | Noop ->
      "NOOP"
  | Unknown ->
      "UNKNOWN"
