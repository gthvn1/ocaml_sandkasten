(* Convert a string of ints into a list of int.
   Example:
     "1234" -> [1; 2; 3; 4]
     *)
let string_to_ints s : int list =
  String.to_seq s |> List.of_seq
  |> List.map (fun c -> int_of_char c - int_of_char '0')

(* Find x and if it exists return the rest of the list
   Example:
     [1; 2; 3; 4; 2; 3; 4] 4 => (Some 4, [2; 3; 4])
     [1; 2; 3; 4; 2; 3; 4] 8 => (None, [])
  *)
let find_x (lst : int list) (value : int) : int option * int list =
  let rec aux = function
    | [] ->
        (None, [])
    | x :: xs ->
        if x = value then (Some x, xs) else aux xs
  in
  aux lst

(*
  Examples:
    In 987654321111111, => 98
    In 811111111111119, => 89
    In 234234234234278, => 78
    In 818181911112111, => 92
*)
let find_largest (lst : int list) : int =
  if List.length lst < 2 then failwith "At least two items are expected" ;
  let rec aux idx a_opt b_opt l =
    if idx < 0 then failwith "cannot be reached be keep it to catch bug" ;
    match find_x l idx with
    | None, _ ->
        aux (idx - 1) a_opt b_opt l
    | Some v, rest -> (
      match (a_opt, b_opt) with
      | None, None ->
          (* Be carfull if the highest is the last one of the list, we cannot
           build largest but as it is the bigger number keep it as the second
           number. *)
          if List.length rest = 0 then aux (idx - 1) None (Some v) l
          else aux idx (Some v) None rest
      | None, Some b ->
          (10 * v) + b
      | Some a, None ->
          (10 * a) + v
      | _ ->
          failwith "unreachable" )
  in
  aux 9 None None lst

(* Part 2: another approach: filter starting from 1 until have a list equal
   or less in lenght than expected number of digits and add the missing
   part once filtered too much... *)

type pos = int * int

(* take a string that is numbers and return a list of tuple that
   is (idx, val).
   Example:
     "123" -> [ (0, 1); (1, 2); (2; 3)]*)
let string_to_tuple (s : string) : pos list =
  let l = string_to_ints s in
  let rec aux acc idx = function
    | [] ->
        List.rev acc
    | x :: xs ->
        aux ((idx, x) :: acc) (idx + 1) xs
  in
  aux [] 0 l

let filter_tuple (value : int) (l : pos list) : pos list * pos list =
  let rec aux acc1 acc2 = function
    | [] ->
        (List.rev acc1, List.rev acc2)
    | ((_, x) as t) :: xs ->
        if x = value then aux acc1 (t :: acc2) xs else aux (t :: acc1) acc2 xs
  in
  aux [] [] l

module D2 = Utils.Openday (struct
  let filename = "aoc2025/files/day03.txt"
end)

let part1 () =
  let input = D2.get_string_list () in
  let l = List.map (fun s -> string_to_ints s |> find_largest) input in
  let output = List.fold_left (fun acc x -> acc + x) 0 l in
  Printf.printf "Day3 part1: %d" output
