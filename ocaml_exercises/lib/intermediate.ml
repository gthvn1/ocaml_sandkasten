(* ----------- IS PRIME --------- *)
let is_prime (n : int) : bool =
  let rec aux = function
    | [] -> true
    | x :: xs ->
        if x = n then true
        else if n mod x = 0 then false
        else aux (List.filter (fun i -> i mod x <> 0) xs)
  in
  if n < 2 then false
  else
    let boundery = float_of_int n |> sqrt |> ceil |> int_of_float in
    aux (List.init boundery (fun x -> x + 2))

(* List.init 100 (fun x -> x) *)
(* |> List.iter (fun x -> if is_prime x then Printf.printf "%d " x) *)

let hello () =
  print_endline
    "Intermediaire: https://ocaml.org/exercises?difficulty_level=intermediate"
