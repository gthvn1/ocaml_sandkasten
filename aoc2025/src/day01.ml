type dir = Left | Right

let get_rotation (s : string) : (dir * int) option =
  try
    let first_char = String.sub s 0 1 in
    let rest = String.sub s 1 (String.length s - 1) in
    match (first_char, int_of_string rest) with
    | "l", x ->
        Some (Left, x)
    | "L", x ->
        Some (Left, x)
    | "r", x ->
        Some (Right, x)
    | "R", x ->
        Some (Right, x)
    | _ ->
        None
  with _ -> None

let normalize (x : int) : int * int = (x / 100, x mod 100)

(* Rotate the dial according to rotation.
   For example if value is 11, "R8" will generate (19, 0)
   But "L11" will return (0, 1)
   The last value is the number of times to hit 0. *)
let rotate (value : int) (rotation : string) : int * int =
  match get_rotation rotation with
  | None ->
      failwith "Unable to get the rotation"
  | Some (d, x) -> (
      (* normalize x *)
      let n, x' = normalize x in
      match d with
      | Left ->
          let wrap = if value - x' <= 0 then 1 else 0 in
          let v' = (value - x' + 100) mod 100 in
          (v', n + wrap)
      | Right ->
          let wrap = if value + x' >= 100 then 1 else 0 in
          let v' = (value + x') mod 100 in
          (v', n + wrap) )

let part1 () = print_endline "TODO: day1"
