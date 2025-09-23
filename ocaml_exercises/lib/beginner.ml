(* ----------- LAST TWO ELEMENTS OF A LIST  --------- *)
let last_two (l : 'a list) : ('a * 'a) option =
  match l with
  | [] | _ :: [] -> None
  | _ ->
      let rec loop = function
        | [ a; b ] -> Some (a, b)
        | _ :: xs -> loop xs
        | _ -> failwith "cannot be reached"
      in
      loop l

let hello () =
  print_endline
    "Beginner: https://ocaml.org/exercises?difficulty_level=beginner"
