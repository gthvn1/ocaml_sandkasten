let rec is_prime' (l : int list) (n : int) : bool =
  match l with
  | [] ->
      true
  | x :: xs ->
      if n = x then true
      else if n mod x = 0 then false
      else is_prime' (List.filter (fun i -> i mod x <> 0) xs) n

(* [is_prime n] returns true if n is prime, false otherwise. *)
let is_prime (n : int) : bool =
  (* Start by easy cases *)
  if n = 0 || n = 1 then false
  else if n = 2 then true
  else if n mod 2 = 0 then false
  else
    (* now we can iterate using kind of erathostene cribble *)
    let n' = float_of_int n |> sqrt |> ceil |> int_of_float in
    let l = List.init n' (fun i -> (2 * i) + 3) in
    is_prime' l n
