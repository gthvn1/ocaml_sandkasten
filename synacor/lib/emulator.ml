type state = {prog: bytes; ip: int (* Instruction pointer *)}

type chunk = char * char option * char option * char option

let read_byte_opt (b : bytes) (pos : int) : char option =
  try Some (Bytes.get b pos) with _ -> None

(** [decode state] returns the four bytes (chunk) under IP that is the
    maximum size of an instruction. If only one byte remains the last
    three can be None. If there is no byte an error is raised. *)
let fetch state : chunk * state =
  match read_byte_opt state.prog state.ip with
  | None ->
      failwith "Failed to fetch instruction"
  | Some i ->
      ( ( i
        , read_byte_opt state.prog state.ip
        , read_byte_opt state.prog state.ip
        , read_byte_opt state.prog state.ip )
      , state )

let decode (fetch_step : chunk * state) : Insn.t * state =
  let (c1, _, _, _), state = fetch_step in
  match Insn.of_char c1 with
  | Noop ->
      (Noop, state)
  | Out ->
      failwith "TODO: out"
  | Halt ->
      (Halt, state)
  | Unknown ->
      failwith (Printf.sprintf "TODO: unknown %c" c1)

let execute (decode_step : Insn.t * state) : state =
  match decode_step with
  | Noop, _ ->
      failwith "todo: execute NOOP"
  | Halt, _ ->
      failwith "todo: execute HALT"
  | Out, _ ->
      failwith "todo: execute OUT"
  | Unknown, _ ->
      failwith "ERROR: unknown instruction to execute"

let run (prog : bytes) : unit =
  let mem = Memory.load prog in
  Memory.dump mem ;
  let init_state = {prog; ip= 0} in
  let new_state = init_state |> fetch |> decode |> execute in
  Printf.printf "Ends at instruction pointer %x\n" new_state.ip
