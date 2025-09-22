(* [odd_or_even s] read two integers n and m from [s].
   Given 2 numbers N and M add both the numbers and
   check whether the sum is odd or even.
   Raise an exception if the string is not well formated. *)
let odd_or_even (s : string) : string =
  let input = String.split_on_char ' ' s |> List.map int_of_string in
  let n = List.nth input 0 in
  let m = List.nth input 1 in
  if (n + m) mod 2 = 0 then "even" else "odd"
