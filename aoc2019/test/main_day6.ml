let () =
  let orbits = List.map Day6.parse Day6.input in
  let map = Day6.map_of_orbits orbits in
  let total = Day6.get_total_orbits map in
  print_int total;
  print_newline ()
