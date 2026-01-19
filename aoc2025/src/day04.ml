let map =
  [
    "..@@.@@@@."
  ; "@@@.@.@.@@"
  ; "@@@@@.@.@@"
  ; "@.@@@@..@."
  ; "@@.@@@@.@@"
  ; ".@@@@@@@.@"
  ; ".@.@.@.@@@"
  ; "@.@@@.@@@@"
  ; ".@@@@@@@@."
  ; "@.@.@@@.@."
  ]
  |> List.map (fun s -> String.to_seq s |> List.of_seq)

let arr = Array.of_list (List.map Array.of_list map)

let get_neigh ~(row : int) ~(col : int) arr : char list =
  (* there are 6 potential neighbors*)
  List.fold_left
    (fun acc (x, y) ->
      try
        let n = arr.(x).(y) in
        n :: acc
      with _ -> acc)
    []
    [
      (row - 1, col - 1)
    ; (row - 1, col)
    ; (row - 1, col + 1)
    ; (row, col - 1)
    ; (row, col + 1)
    ; (row + 1, col - 1)
    ; (row + 1, col)
    ; (row + 1, col + 1)
    ]

let get_neigh_is_rolls ~row ~col arr =
  get_neigh ~row ~col arr |> List.filter (fun c -> c = '@')

let access_forklift a : int =
  let v = ref 0 in
  let row = Array.length a in
  let col = Array.length a.(0) in
  for r = 0 to row - 1 do
    for c = 0 to col - 1 do
      if a.(r).(c) = '@' && List.length (get_neigh_is_rolls ~row:r ~col:c a) < 4
      then v := !v + 1
    done
  done;
  !v

module D4 = Utils.Openday (struct
  let filename = "aoc2025/files/day04.txt"
end)

let part1 () =
  let arr =
    D4.get_string_list ()
    |> List.map (fun s -> String.to_seq s |> List.of_seq |> Array.of_list)
    |> Array.of_list
  in
  Printf.printf "Day4: %d" (access_forklift arr)
