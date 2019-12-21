grade_only [1;2;3;4;5;6;7] ;;

let rec loop p f x = if p x then x else loop p f (f x);;

let rec exists p = function
  | [] -> false
  | x::xs -> if p x then true else exists p xs

let rec find p = function
  | [] -> raise NotFound
  | x::xs -> if p x then x else find p xs

(* --- Part A: A Generic Problem Solver --- *)

let near x = [x - 2; x - 1; x; x + 1; x + 2] ;;

let rec flat_map r = function
  | [] -> []
  | x::xs -> (r x) @ (flat_map r xs)

let rec iter_rel (rel:'a rel) (n:int) : 'a rel = fun x -> match n with
  | 0  -> [x]
  | n' -> flat_map rel (iter_rel rel (n - 1) x)

let solve (r:'e rel) (p:'e prop) (x:'e) =
  let rec solve' r p l =
    let rec find_solution = function
      | [] -> None
      | x::xs -> if p x then Some x else find_solution xs in
    match find_solution l with
    | None -> solve' r p (flat_map r l)
    | Some x -> x
  in
  solve' r p [x]

let solve_path r p x =
  "Replace this string with your implementation." ;;

let archive_map opset r (s, l) =
  "Replace this string with your implementation." ;;

let solve' opset r p x =
  "Replace this string with your implementation." ;;

let solve_path' opset r p x =
  "Replace this string with your implementation." ;;

let solve_puzzle p opset c =
  "Replace this string with your implementation." ;;

(* --- Part B: A Solver for Klotski --- *)

let final board =
  "Replace this string with your implementation." ;;

let move_piece board piece { drow; dcol } =
  "Replace this string with your implementation." ;;

let possible_moves board =
  "Replace this string with your implementation." ;;

module BoardSet = Set.Make (struct
    type t = board
    let compare b1 b2 =
      failwith "Replace this with your implementation." ;;
  end)

let solve_klotski initial_board =
  "Replace this string with your implementation." ;;
