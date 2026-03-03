let rec digits (n : int) : int list =
  if n < 10 then [ n ] else digits (n / 10) @ [ n mod 10 ]

let is_valid1 (n : int) : bool =
  let rec aux l prev has_double =
    match l with
    | [] -> has_double
    | x :: xs ->
        let has_double = if not has_double then x = prev else has_double in
        if x >= prev then aux xs x has_double else false
  in
  aux (digits n) (-1) false

let is_valid2 n =
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
