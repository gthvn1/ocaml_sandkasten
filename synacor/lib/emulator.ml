type state = {prog: bytes; ip: int (* Instruction pointer *)}

let read_instruction state : Insn.t option =
  try
    let i = Bytes.get state.prog state.ip |> Insn.of_char in
    Printf.printf "Debug: find %s\n" (Insn.to_string i) ;
    Some i
  with _ -> None

let run (prog : bytes) : unit =
  let init_state = {prog; ip= 0} in
  let first_insn = read_instruction init_state in
  match first_insn with
  | Some i ->
      Printf.printf "Read first instruction: %s\n" (Insn.to_string i)
  | None ->
      Printf.printf "Rom seems empty, failed to read first instruction\n"
