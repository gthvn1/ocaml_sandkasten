let read_input () : int =
  print_string "Enter a number: ";
  read_int ()

let x =
  read_input () |> fun a ->
  read_input () |> fun b -> a + b

let () = print_endline (string_of_int x)
