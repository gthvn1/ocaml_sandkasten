type state = {mem: Memory.t; ip: int (* Instruction pointer *)}

type chunk = int * int option * int option * int option

let read_mem_opt (mem : int array) (pos : int) : int option =
  Memory.read mem ~addr:pos

(** [decode state] returns the four word (chunk) under IP that is the
    maximum size of an instruction. If only one word remains the last
    three can be None. If there is no word we are out or memory and
    an error is raised. *)
let fetch state : chunk * state =
  match read_mem_opt state.mem state.ip with
  | None ->
      failwith (Printf.sprintf "Out of memory: trying access Mem[0x%x]" state.ip)
  | Some v ->
      ( ( v
        , read_mem_opt state.mem state.ip
        , read_mem_opt state.mem state.ip
        , read_mem_opt state.mem state.ip )
      , state )

let decode (fetch_step : chunk * state) : Insn.t * state =
  let (m1, _, _, _), state = fetch_step in
  match Insn.of_int m1 with
  | Noop ->
      (Noop, state)
  | Out ->
      failwith "TODO: out"
  | Halt ->
      (Halt, state)
  | Unknown ->
      failwith (Printf.sprintf "TODO: unknown opcode 0x%02x" m1)

let execute (decode_step : Insn.t * state) : state =
  match decode_step with
  | Noop, state ->
      failwith (Printf.sprintf "TODO: execute Noop at 0x%02x" state.ip)
  | Halt, state ->
      failwith (Printf.sprintf "TODO: execute Halt at 0x%02x" state.ip)
  | Out, state ->
      failwith (Printf.sprintf "TODO: execute Out at 0x%02x" state.ip)
  | Unknown, state ->
      failwith (Printf.sprintf "ERROR: unknown instruction to execute at 0x%02x" state.ip)

let run (prog : bytes) : unit =
  let mem = Memory.load prog in
  Memory.dump mem ;
  let init_state = {mem; ip= 0} in
  let new_state = init_state |> fetch |> decode |> execute in
  Printf.printf "Ends at instruction pointer %x\n" new_state.ip
