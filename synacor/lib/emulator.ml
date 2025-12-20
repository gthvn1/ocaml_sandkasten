type state = {prog: bytes; ip: int (* Instruction pointer *)}

type chunk = char * char option * char option * char option

let read_byte_opt (b : bytes) (pos : int) : char option =
  try Some (Bytes.get b pos) with _ -> None

(** [decode state] returns the four bytes (chunk) under IP that is the
    maximum size of an instruction. If only one byte remains the last
    three can be None. If there is no byte None is returned. *)
let fetch state : chunk option * state =
  match read_byte_opt state.prog state.ip with
  | None ->
      (None, state)
  | Some i ->
      ( Some
          ( i
          , read_byte_opt state.prog state.ip
          , read_byte_opt state.prog state.ip
          , read_byte_opt state.prog state.ip )
      , state )

let decode (fetch_step : chunk option * state) : Insn.t option * state =
  let chunk, state =
    match fetch_step with
    | None, _ ->
        failwith "Failed to fetch instruction"
    | Some c, s ->
        (c, s)
  in
  let _ = chunk in
  let _ = state in
  failwith "TODO: decode insn"

let execute (decode_step : Insn.t option * state) : state =
  let _ = decode_step in
  failwith "todo: execute insn"

let run (prog : bytes) : unit =
  let init_state = {prog; ip= 0} in
  let new_state = init_state |> fetch |> decode |> execute in
  Printf.printf "Ends at instruction pointer %x\n" new_state.ip
