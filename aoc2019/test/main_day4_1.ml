let () =
  print_endline "Solving day4 with 1: ";
  let result = ref 0 in
  for n = 158126 to 624574 do
    if Day4.is_valid1 n then result := !result + 1
  done;
  print_int !result;
  print_newline ()
