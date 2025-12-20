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

let dump (mem : t) =
  (* just print first ten value *)
  for i = 0 to 9 do
    Printf.printf "Mem[%02d] : 0x%04X\n" i (Array.get mem i)
  done
