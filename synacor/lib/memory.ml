(*
  - memory is
    - word of 16-bit
    - eight registers
  - We will use int to store them in an array
  - It uses litte-endian representation (low byte, high byte)

  # Layout
    - Memory can be accessed using 0 to 32767 (15 bits)
      - Adding 1 to the last memory word wrapped to 0
    - Regs can be accessed using 32768, 32769, ... 32775.
    - Region from 32776..65535 is invalid
*)

type t = {mem: int Array.t; regs: int Array.t}

(** Constants describing the VM memory layout and register file. *)
module Layout = struct
  (** Total number of addressable memory words.
      Memory addresses range from [0] to [mem_size - 1]. *)
  let mem_size = 32768

  (** Lowest valid memory address. *)
  let mem_min = 0

  (** Highest valid memory address. *)
  let mem_max = mem_size - 1

  (** Number of general-purpose registers. *)
  let num_regs = 8

  (** Lowest address mapped to a register. *)
  let reg_min = mem_size

  (** Highest address mapped to a register. *)
  let reg_max = reg_min + num_regs - 1
end

let is_mem (addr : int) : bool =
  addr >= Layout.mem_min && addr <= Layout.mem_max

let is_reg (addr : int) : bool =
  addr >= Layout.reg_min && addr <= Layout.reg_max

(**  [load rom] loads the given binary [rom] at the beginning of
     the memory. It raises an exception if [rom] is too big. *)
let load (rom : bytes) : t =
  let rec aux mem pos =
    try
      let low_byte = Bytes.get rom pos |> Char.code in
      let high_byte = Bytes.get rom (pos + 1) |> Char.code in
      aux ((low_byte + (high_byte lsl 8)) :: mem) (pos + 2)
    with _ -> List.rev mem |> Array.of_list
  in
  {mem= aux [] 0; regs= Array.make Layout.num_regs 0}

(** [write mem addr value] sets register at address [addr] of [mem] to
    [value]. It raises an error if address is not a register. *)
let write (memory : t) ~(addr : int) ~(value : int) : unit =
  if value > 65535 then
    failwith
      (Printf.sprintf "Cannot write %d into register (too big value)" value) ;
  if not (is_reg addr) then
    failwith
      (Printf.sprintf "Error: Try to write a register at address 0x%x" addr) ;
  let idx = addr - Layout.reg_min in
  Array.set memory.regs idx value

(** [read mem addr] returns the value at address [addr].
    Between 0 to 32767 it accesses the RAM, from 32768, 32769, ... 32775
    it is the registers and above 32776 it is not memory. *)
let read (memory : t) ~(addr : int) : int =
  if is_mem addr then memory.mem.(addr)
  else if is_reg addr then
    let idx = addr - Layout.reg_min in
    memory.regs.(idx)
  else failwith "Out of memory"

(** [to_str mem] return a string that is memory around the current
    instruction pointer. It is used for debugging. *)
let to_str ~(memory : t) ~(addr : int) : string =
  if addr > Layout.reg_max then failwith "Out of memory" ;
  let mem_size = Array.length memory.mem in
  if mem_size = 0 then "Empty mem"
  else
    (* start by printing memory around addr *)
    let start = max 0 (addr - 5) in
    let fin = min (mem_size - 1) (addr + 5) in
    let output = ref "" in
    for i = start to fin do
      let mem_str =
        if addr = i then Printf.sprintf "=> Mem[%03d]:0x%04X\n" i memory.mem.(i)
        else Printf.sprintf "   Mem[%03d]:0x%04X\n" i memory.mem.(i)
      in
      output := !output ^ mem_str
    done ;
    (* and print registers *)
    output := !output ^ "\nRegs: " ;
    for i = 0 to 7 do
      let reg_str = Printf.sprintf "[%d]=0x%04x " i memory.regs.(i) in
      output := !output ^ reg_str
    done ;
    !output
