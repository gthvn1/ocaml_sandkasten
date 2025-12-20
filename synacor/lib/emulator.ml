type state = Halted | Running

type vm = {mem: Memory.t; ip: int (* Instruction pointer *); state: state}

let read_mem_opt (mem : int array) (pos : int) : int option =
  Memory.read mem ~addr:pos

(** [decode vm] returns the four word (chunk) under IP that is the
    maximum size of an instruction. If only one word remains the last
    three can be None. If there is no word we are out or memory and
    an error is raised. *)
let fetch vm : Insn.chunk * vm =
  match read_mem_opt vm.mem vm.ip with
  | None ->
      failwith (Printf.sprintf "Out of memory: trying access Mem[0x%x]" vm.ip)
  | Some v ->
      ( ( v
        , read_mem_opt vm.mem (vm.ip + 1)
        , read_mem_opt vm.mem (vm.ip + 2)
        , read_mem_opt vm.mem (vm.ip + 3) )
      , vm )

let decode (fetch_step : Insn.chunk * vm) : Insn.t * vm =
  let ((c1, _, _, _) as chunk), vm = fetch_step in
  match Insn.decode chunk with
  | None ->
      failwith
        (Printf.sprintf "Failed to decode instruction 0x%02x at 0x%02x" c1 vm.ip)
  | Some (insn, size) ->
      (insn, {vm with ip= vm.ip + size})

let execute (decode_step : Insn.t * vm) : vm =
  match decode_step with
  | Noop, vm ->
      vm
  | Halt, vm ->
      {vm with state= Halted}
  | Out c, vm ->
      Printf.printf "%c%!" c ; vm
  | Unknown, vm ->
      failwith
        (Printf.sprintf "ERROR: unknown instruction to execute at 0x%02x)" vm.ip)

let run (prog : bytes) : unit =
  let mem = Memory.load prog in
  let rec loop vm =
    prerr_endline "   ----- MEM -----" ;
    prerr_endline (Memory.to_str ~mem:vm.mem ~pos:vm.ip) ;
    if vm.state = Halted then Printf.printf "VM halted"
    else vm |> fetch |> decode |> execute |> loop
  in
  loop {mem; ip= 0; state= Running}
