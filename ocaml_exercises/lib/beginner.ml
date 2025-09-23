(* ----------- TAIL OF A LIST  --------- *)
let rec last (l : 'a list) : 'a option =
  match l with [] -> None | x :: [] -> Some x | _ :: xs -> last xs

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

(* ----------- NTH ELEMENT OF A LIST  --------- *)
let rec at (n : int) (l : 'a list) : 'a option =
  match l with [] -> None | x :: xs -> if n = 0 then Some x else at (n - 1) xs

(* ----------- RUN-LENGTH ENCODING  --------- *)

let encode (s : string list) : (int * string) list =
  let rec aux acc count = function
    | [] -> []
    | [ a ] -> (count + 1, a) :: acc
    | a :: (b :: _ as t) ->
        if a = b then aux acc (count + 1) t else aux ((count + 1, a) :: acc) 0 t
  in
  List.rev (aux [] 0 s)

(* encode [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ] *)
(* Expect: [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")] *)

let hello () =
  print_endline
    "Beginner: https://ocaml.org/exercises?difficulty_level=beginner"
