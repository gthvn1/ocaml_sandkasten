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

let () =
		print_is_happy 32;
