(* ----------- FLATTEN A LIST  --------- *)
type 'a node = One of 'a | Many of 'a node list

let flatten (lst : 'a node list) : 'a list =
  let rec aux (acc : 'a list) (l : 'a node list) =
    match l with
    | [] -> acc
    | One x :: xs -> aux (x :: acc) xs
    | Many x :: xs -> aux (aux acc x) xs
  in
  List.rev (aux [] lst)

let a = One "a"
let b = One "b"
let c = One "c"
let d = One "d";;

Many [ c; d ];;
flatten [ One "a"; Many [ One "b"; Many [ One "c"; One "d" ]; One "e" ] ]
(* Expected : string list = ["a"; "b"; "c"; "d"; "e"] *)
;;

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
