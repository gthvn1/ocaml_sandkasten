let rec digits (n : int) : int list =
  if n < 10 then [ n ] else digits (n / 10) @ [ n mod 10 ]

let is_valid_part_1 (n : int) : bool =
  let rec aux l prev has_double =
    match l with
    | [] -> has_double
    | x :: xs ->
        let has_double = if not has_double then x = prev else has_double in
        if x >= prev then aux xs x has_double else false
  in
  aux (digits n) (-1) false

let is_valid_part_1' n =
  let rec aux n prev has_double =
    if n = 0 then has_double
    else
      let digit = n mod 10 in
      if digit > prev then false
      else
        let has_double = has_double || digit = prev in
        aux (n / 10) digit has_double
  in
  aux n 10 false

(** [max_consecutive x n] returns the maximum number of times the digit [x]
    appears consecutively in the number [n].

    Examples:
    - max_consecutive 1 1234 -> returns 1
    - max_consecutive 1 5678 -> returns 0
    - max_consecutive 1 1123 -> returns 2
    - max_consecutive 1 111211 -> returns 3
    - max_consecutive 1 112111 -> returns 3 *)
let max_consecutive (x : int) (n : int) : int =
  let rec loop (max_streak : int) (current_streak : int) = function
    | v when v < 10 ->
        max max_streak (if v = x then current_streak + 1 else current_streak)
    | v ->
        let digit = v mod 10 in
        if digit = x then loop max_streak (current_streak + 1) (v / 10)
        else loop (max max_streak current_streak) 0 (v / 10)
  in
  loop 0 0 n

let is_part_two_complient n =
  List.init 10 (fun x -> x)
  |> List.filter_map (fun x -> if max_consecutive x n = 2 then Some x else None)
  |> List.is_empty |> not
