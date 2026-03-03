let () =
  let open Day6 in
  In_channel.open_text "inputs/day6.txt"
  |> In_channel.input_lines |> List.map parse |> map_of_orbits
  |> get_total_orbits |> print_int;
  print_newline ()
