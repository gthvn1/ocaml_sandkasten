type dir = Left | Right

let get_rotation (s : string) : (dir * int) option =
  try
    let dir_str = String.sub s 0 1 in
    let steps = String.sub s 1 (String.length s - 1) |> int_of_string in
    match dir_str with
    | "l" | "L" ->
        Some (Left, steps)
    | "r" | "R" ->
        Some (Right, steps)
    | _ ->
        None
  with _ -> None

let normalize (x : int) : int * int = (x / 100, x mod 100)

(* Rotate the dial according to rotation.
   For example if value is 11, "R8" will generate (19, 0)
   But "L11" will return (0, 1)
   The last value is the number of times to hit 0. *)
let rotate1 (value : int) (rotation : string) : int * int =
  match get_rotation rotation with
  | None ->
      failwith "Unable to get the rotation"
  | Some (d, x) -> (
      (* normalize x *)
      let x' = x mod 100 in
      match d with
      | Left ->
          let v' = (value - x' + 100) mod 100 in
          (v', if v' = 0 then 1 else 0)
      | Right ->
          let v' = (value + x') mod 100 in
          (v', if v' = 0 then 1 else 0) )

let rotate2 (value : int) (rotation : string) : int * int =
  (* Printf.printf "\npos is %d \n" value ; *)
  match get_rotation rotation with
  | None ->
      failwith "Unable to get the rotation"
  | Some (d, x) -> (
      (* normalize x *)
      let n, x' = normalize x in
      match d with
      | Left ->
          let wrap = if value <> 0 && value - x' <= 0 then 1 else 0 in
          let v' = (value - x' + 100) mod 100 in
          (* Printf.printf "Left rotate to %d, n %d , wrapped %d\n" v' n wrap ; *)
          (v', n + wrap)
      | Right ->
          let wrap = if value <> 0 && value + x' >= 100 then 1 else 0 in
          let v' = (value + x') mod 100 in
          (* Printf.printf "Right rotate to %d, n %d, wrapped %d\n" v' n wrap ; *)
          (v', n + wrap) )

let rotations_sample =
  ["L68"; "L30"; "R48"; "L5"; "R60"; "L55"; "L1"; "L99"; "R14"; "L82"]

module Day1 = struct
  let filename = "aoc2025/files/day01.txt"
end

module D1 = Utils.Openday (Day1)

let part1 () =
  let rotations = D1.get_string_list () in
  let final_pos, total_hits =
    List.fold_left
      (fun (pos, hits) str ->
        let new_pos, h = rotate1 pos str in
        (new_pos, h + hits) )
      (50, 0) rotations
  in
  Printf.printf "reached %d and hits 0 %d times\n" final_pos total_hits

let part2 () =
  let rotations = D1.get_string_list () in
  let final_pos, total_hits =
    List.fold_left
      (fun (pos, hits) str ->
        let new_pos, h = rotate2 pos str in
        (new_pos, h + hits) )
      (50, 0) rotations
  in
  Printf.printf "reached %d and hits 0 %d times\n" final_pos total_hits
