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

let rec digits_from_string = function
  | "" -> []
  | s  -> (digit_of_char (String.get s 0)) ::
          (digits_from_string (String.sub s 1 ((String.length s) - 1)));;

let sum_of_squares s =
  List.fold_left (fun x y -> x + y * y) 0 (digits_from_string s);;

let rec list_contains l a = match l with
  | [] -> false
  | x::xs -> (if a = x then true
              else list_contains xs a);;

let string_is_happy s =
  let rec aux l s = match sum_of_squares s with
    | 1 -> true
    | y -> (if list_contains l y then false
            else (aux (y::l) (string_of_int y))) in
  aux [] s;;

let print_is_happy s =
  print_string s;
  if string_is_happy s then print_endline " :)"
  else print_endline " :(";;
