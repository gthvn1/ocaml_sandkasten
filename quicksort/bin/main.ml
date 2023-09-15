module Q = Quicksort

let rec string_of_list = function
  | [] -> ""
  | x::[] -> string_of_int x
  | x::xs -> (string_of_int x) ^ " " ^ (string_of_list xs)

let () =
  let l = [5; 6; 12; 4; 75; 43] in
  print_string "Sorting: ";
  print_string (string_of_list l);
  print_endline "";

  let sorted = Q.quicksort l in
  print_string "Sorted: ";
  print_string (string_of_list sorted);
  print_endline ""
