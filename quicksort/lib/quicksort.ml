(*
  [9, 7, 5, 11, 12, 2, 14, 3, 10, 6]

  => [5, 2, 3] 6 [12, 7, 14, 9, 10, 11]
  => [2] 3 [5] 6 [7, 9, 10] 11 [12, 14]
  => 2 3 5 6 [7, 9] 10 11 [12] 14
  => 2 3 5 6 [7] 9 10 11 12 14
  => 2 3 5 6 7 9 10 11 12 14
*)

let rec sort_using_fn (x: int) (l: int list) (f: int -> int -> bool): int list = match l with
  | [] -> []
  | y::ys -> if f y x then y :: sort_using_fn x ys f
                      else sort_using_fn x ys f

let rec quicksort = function
  | [] -> []
  | x::[] -> [x]
  | x::xs ->
      let left  = sort_using_fn x xs ( <= ) in
      let right = sort_using_fn x xs ( > ) in
      (quicksort left) @ [x] @ (quicksort right)
