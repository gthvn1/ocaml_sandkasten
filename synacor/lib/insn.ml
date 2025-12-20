type t = Halt | Out | Noop | Unknown

let of_int (v : int) : t =
  match v with 0 -> Halt | 19 -> Out | 21 -> Noop | _ -> Unknown

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
