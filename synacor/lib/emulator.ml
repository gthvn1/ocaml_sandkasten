type state = Halted | Running

type mode = Normal | Step

type vm =
  { mem: Memory.t
  ; ip: int (* Instruction pointer *)
  ; state: state
  ; breakpoint: int option
  ; mode: mode }

let read_mem_opt (mem : Memory.t) (addr : int) : int option =
  try Some (Memory.read mem ~addr)
  with Failure msg ->
    Printf.printf "Warning: failed to read memory at 0x%x: %s" addr msg ;
    None

(** [decode vm] returns four words (chunk) under IP that is the
    maximum size of an instruction. If only one word remains the last
    three can be None. If there is no word we are out of memory and
    an error is raised. *)
let fetch vm : Insn.chunk * vm =
  match read_mem_opt vm.mem vm.ip with
  | None ->
      failwith
        (Printf.sprintf "Out of memory: trying to access Mem[0x%x]" vm.ip)
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

let disassemble (decode_step : Insn.t * vm) : vm =
  let insn, vm = decode_step in
  Printf.printf "%05x| %s\n" vm.ip (Insn.to_string insn) ;
  vm

let execute (decode_step : Insn.t * vm) : vm =
  match decode_step with
  | Add (_a, _b, _c), _vm ->
      failwith "Execute add"
  | Halt, vm ->
      {vm with state= Halted}
  | Jmp addr, vm ->
      {vm with ip= addr}
  | Jt (value, addr), vm ->
      (* If addr is a reg address read its value from memory *)
      let open Memory in
      let v = if is_reg value then read vm.mem ~addr:value else value in
      if v <> 0 then {vm with ip= addr} else vm
  | Jf (value, addr), vm ->
      let open Memory in
      let v = if is_reg value then read vm.mem ~addr:value else value in
      if v = 0 then {vm with ip= addr} else vm
  | Noop, vm ->
      vm
  | Out c, vm ->
      Printf.printf "%c%!" c ; vm
  | Set (addr, value), vm ->
      Memory.write vm.mem ~addr ~value ;
      vm
  | Unknown, vm ->
      failwith
        (Printf.sprintf "ERROR: unknown instruction to execute at 0x%02x)" vm.ip)

(* [with_raw_input f] allows to execute function [f] while disabling
   line buffering to avoid typing enter. *)
let with_raw_input f vm =
  let open Unix in
  let fd = descr_of_in_channel In_channel.stdin in
  let old = tcgetattr fd in
  let raw = {old with c_icanon= false} in
  tcsetattr fd TCSANOW raw ;
  try
    let new_vm = f vm in
    tcsetattr fd TCSANOW old ; new_vm
  with e -> tcsetattr fd TCSANOW old ; raise e

let rec prompt vm =
  Printf.printf "[s]tep [p]rint [c]ontinue [q]uit\n> %!" ;
  match In_channel.input_char stdin with
  | None ->
      failwith "not expected"
  | Some 'q' | Some 'Q' ->
      exit 0
  | Some 'p' | Some 'P' ->
      print_newline () ;
      prerr_endline "   ----- MEM -----" ;
      prerr_endline (Memory.to_str ~memory:vm.mem ~addr:vm.ip) ;
      prompt vm
  | Some 's' | Some 'S' ->
      print_newline () ; {vm with mode= Step}
  | Some 'c' ->
      print_newline () ; {vm with mode= Normal}
  | Some cmd ->
      Printf.printf "\nUnkown command %c\n%!" cmd ;
      prompt vm

let is_breakpoint (vm : vm) (breakpoint : int option) : bool =
  match breakpoint with None -> false | Some v -> vm.ip = v

let run ?(debug_mode = false) ?(breakpoint : int option = None) ?(disas = false)
    (prog : bytes) : unit =
  let mem = Memory.load prog in
  let rec loop vm =
    if vm.state = Halted then (Printf.printf "VM halted" ; exit 0) ;
    let vm =
      if vm.mode = Step || is_breakpoint vm breakpoint then
        with_raw_input prompt vm
      else vm
    in
    if disas then vm |> fetch |> decode |> disassemble |> loop
    else vm |> fetch |> decode |> execute |> loop
  in
  loop
    { mem
    ; ip= 0
    ; state= Running
    ; breakpoint
    ; mode= (if debug_mode then Step else Normal) }
