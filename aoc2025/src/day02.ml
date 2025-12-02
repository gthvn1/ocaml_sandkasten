(* split string into chunks of string of size [chunk_size].
   Examples:
     split_string "123123" 3 -> ["123", "123"]
  If it cannot be splitted an error is raised
   *)
let split_string (s : string) (chunk_size : int) : string list =
  let rec aux acc str =
    if String.length str = 0 then List.rev acc
    else if String.length str = chunk_size then List.rev (str :: acc)
    else
      aux
        (String.sub str 0 chunk_size :: acc)
        (String.sub str chunk_size (String.length str - chunk_size))
  in
  aux [] s

(* return true if a sequence of n digits is repeated_twice.
   Example:
     for n = 1: 6464 -> false
     for n = 2: 6464 -> true
     for n > 2: false

     for n = 1: 123123 -> false
     for n = 2: 123123 -> false
     for n = 3: 123123 -> true
     for n > 3: false
  *)
let repeated_fixed_size (s : string) (len : int) : bool =
  (* size of the string must be at least 2 times len *)
  if String.length s < 2 * len then false
  else if
    (* and it must be a multiple of len *)
    String.length s mod len <> 0
  then false
  else
    let chunks = split_string s len in
    if List.length chunks < 2 then false
    else
      let h = List.hd chunks in
      let l = List.filter (fun s -> s <> h) (List.tl chunks) in
      List.length l = 0
(* We need to split string into: 0 - len, len - 2*len, 2*len - 3*len ...
  For example for len = 2: 0->2 ; 2->4; 4->6 until reaching lentth of string
*)

(* return true if a sequence of digits is repeated twice.
   Examples:
     6464 -> true
     123123 -> true
     101 -> false
*)
let repeated (s : string) : bool =
  (* We need to check from n = 1 while n < String.length s / 2 *)
  let lim = String.length s / 2 in
  let rec aux n =
    if n <= lim then if repeated_fixed_size s n then true else aux (n + 1)
    else false
  in
  aux 1

let part1 () = print_endline "TODO"
