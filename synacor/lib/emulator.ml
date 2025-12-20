type state = Halted | Running

type vm = {mem: Memory.t; ip: int (* Instruction pointer *); state: state}

type chunk = int * int option * int option * int option

let read_mem_opt (mem : int array) (pos : int) : int option =
  Memory.read mem ~addr:pos

(** [decode vm] returns the four word (chunk) under IP that is the
    maximum size of an instruction. If only one word remains the last
    three can be None. If there is no word we are out or memory and
    an error is raised. *)
let fetch vm : chunk * vm =
  match read_mem_opt vm.mem vm.ip with
  | None ->
      failwith (Printf.sprintf "Out of memory: trying access Mem[0x%x]" vm.ip)
  | Some v ->
      ( ( v
        , read_mem_opt vm.mem vm.ip
        , read_mem_opt vm.mem vm.ip
        , read_mem_opt vm.mem vm.ip )
      , vm )

let decode (fetch_step : chunk * vm) : Insn.t * vm =
  let (m1, _, _, _), vm = fetch_step in
  match Insn.of_int m1 with
  | Noop ->
      (* Noop just update ip*)
      (Noop, {vm with ip= vm.ip + 1})
  | Out ->
      failwith "TODO: decode out"
  | Halt ->
      (Halt, {vm with state= Halted})
  | Unknown ->
      failwith (Printf.sprintf "TODO: unknown opcode 0x%02x" m1)

let execute (decode_step : Insn.t * vm) : vm =
  match decode_step with
  | Noop, vm ->
      vm
  | Halt, vm ->
      (* Decoder has set the state so nothing to do here*)
      vm
  | Out, vm ->
      failwith (Printf.sprintf "TODO: execute Out at 0x%02x" vm.ip)
  | Unknown, vm ->
      failwith
        (Printf.sprintf "ERROR: unknown instruction to execute at 0x%02x" vm.ip)

let run (prog : bytes) : unit =
  let mem = Memory.load prog in
  let rec loop vm =
    print_endline "   ----- MEM -----" ;
    print_endline (Memory.to_str ~mem:vm.mem ~pos:vm.ip) ;
    if vm.state = Halted then Printf.printf "VM halted"
    else vm |> fetch |> decode |> execute |> loop
  in
  loop {mem; ip= 0; state= Running}
