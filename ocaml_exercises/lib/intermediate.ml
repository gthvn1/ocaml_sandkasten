(* ----------- FLATTEN --------- *)
type 'a node = One of 'a | Many of 'a node list

let flatten (l : 'a node list) : 'a list =
  let rec loop acc = function
    | [] -> List.rev acc
    | x :: xs -> (
        match x with
        | One x -> loop (x :: acc) xs
        | Many _ ->
            print_endline "TODO: flatten many";
            List.rev acc)
  in
  loop [] l
;;

flatten [ One "a" ];;
flatten [ One 1; Many [ One 2 ] ]

let hello () =
  print_endline
    "Intermediaire: https://ocaml.org/exercises?difficulty_level=intermediate"
