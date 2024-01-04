let () = 
let lst = [Shape.Circle 3.; Shape.Rectangle (2., 3.)] in
List.iter (fun s -> print_endline (string_of_float (Shape.area s))) lst