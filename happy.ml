let rec sum_double_digits x = 
    if x < 10 then x * x
              else let d = x mod 10 in
    (d * d) + sum_double_digits (x / 10);;

let rec list_contains l a = match l with
    | [] -> false
    | x::xs -> if a = x then true
                        else list_contains xs a;;

let is_happy a =
    let rec aux l a = match sum_double_digits a with
        | 1 -> true
        | y -> if list_contains l y then false
                                    else (aux (y::l) y) in
    aux [] a;;

let print_is_happy a =
    print_int a;
    if is_happy a then print_endline " :)"
                  else print_endline " :(";;

(*
 * This works for small number but there is an issue with number
 * like 8773540098233661513.
 * For this we will try another approach
 * - transforme a string to a list of int
 *    "8773540098233661513" -> [8; 7; 7; ... 3] -> 
 * - fold the list to get the sum of square integers
 
exception Not_a_digit;;

let digit_of_char = function
| '1' -> 1
| '2' -> 2
| '3' -> 3
| '4' -> 4
| '5' -> 5
| '6' -> 6
| '7' -> 7
| '8' -> 8
| '9' -> 9
| '0' -> 0
| _ -> raise Not_a_digit;;

let rec string_to_list_of_digits ch = match ch with
| "" -> []
| _ -> (digit_of_char (String.get ch 0)) ::
(string_to_list_of_digits (String.sub ch 1 ((String.length ch) - 1)));;

List.fold_left (fun x y -> x + y*y) 0 (string_to_list_of_digits "123");;
*)

