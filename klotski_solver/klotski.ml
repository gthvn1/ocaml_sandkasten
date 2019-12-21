exception NotFound

type 'a rel = 'a -> 'a list

type 'a prop = 'a -> bool

type ('a, 'set) set_operations = {
  empty : 'set;             (* The empty set *)
  mem: 'a -> 'set -> bool;  (* [mem x s = true] iff [x] is in [s]. *)
  add: 'a -> 'set -> 'set;  (* [add x s] is the set [s] union {x} *)
}

type ('configuration, 'move) puzzle = {
  move: 'configuration -> 'move -> 'configuration;
  possible_moves: 'configuration -> 'move list;
  final: 'configuration -> bool;
}

(* Pieces on the board can be designated as:
 *   - the one 2x2 square: S
 *   - the one 2x1 horizontal rectangle: H
 *   - the four 1x2 vertical rectangle: V0, V1, V2, V3
 *   - the four 1x1 squares: C0, C1, C2, C3
 *)
type piece_kind = S | H | V | C | X

type piece = piece_kind * int

let x = (X, 0) and s = (S, 0) and h = (H, 0)

let (v0, v1, v2, v3) = ((V, 0), (V, 1), (V, 2), (V, 3))

let (c0, c1, c2, c3) = ((C, 0), (C, 1), (C, 2), (C, 3))

let all_pieces = [x; s; h; v0; v1; v2; v3; c0; c1; c2; c3]

type board = (piece array) array

let initial_board =
  [|
    [| v0; s ; s ; v1 |];
    [| v0; s ; s ; v1 |];
    [| v2; h ; h ; v3 |];
    [| v2; c0; c1; v3 |];
    [| c2; x ; x ; c3 |];
  |]

let initial_board_simpler =
  [|
    [| c0; s ; s ; c2 |];
    [| c1; s ; s ; c3 |];
    [| v0; v1; v2; v3 |];
    [| v0; v2; v3; v3 |];
    [| x ; x ; x ; x  |];
  |]

let initial_board_trival =
  [|
    [| x; s; s; x|];
    [| x; s; s; x|];
    [| x; x; x; x|];
    [| x; x; x; x|];
    [| x; x; x; x|];
  |]

(* let's use a record for direction *)
type direction = {dcol: int; drow: int}

type move = Move of piece * direction * board

let move _ (Move (_, _, b)) = b

let rec loop p f x = if p x then x else loop p f (f x)

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
  | n -> flat_map rel (iter_rel rel (n - 1) x)

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

(*
 * TODO: Implement the following function

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

*)
