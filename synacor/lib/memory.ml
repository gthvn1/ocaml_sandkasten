(*
 - memory is word of 16-bit
 - We will use int to store them in an array
 - It uses litte-endian representation (low byte, high byte)
*)

type t = int Array.t

let load (rom : bytes) : t =
  let rec aux mem pos =
    try
      let low_byte = Bytes.get rom pos |> Char.code in
      let high_byte = Bytes.get rom (pos + 1) |> Char.code in
      aux ((low_byte + (high_byte lsl 8)) :: mem) (pos + 2)
    with _ -> List.rev mem |> Array.of_list
  in
  aux [] 0

let read (mem : t) ~(addr : int) : int option =
  try Some (Array.get mem addr) with _ -> None

(** [to_str mem] return a string that is memory around the current instruction
    pointer. It is used for debugging. *)
let to_str ~(mem : t) ~(pos : int) : string =
  let mem_size = Array.length mem in
  if mem_size = 0 then "Empty mem"
  else
    let start = max 0 (pos - 5) in
    let fin = min (mem_size - 1) (pos + 5) in
    let output = ref "" in
    for i = start to fin do
      let mem_str =
        if pos = i then
          Printf.sprintf "=> Mem[%03d]:0x%04X\n" i (Array.get mem i)
        else Printf.sprintf "   Mem[%03d]:0x%04X\n" i (Array.get mem i)
      in
      output := !output ^ mem_str
    done;
    !output
