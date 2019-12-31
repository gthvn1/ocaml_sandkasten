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
    [| v1; v2; v3; v0 |];
    [| v2; v3; v3; v0 |];
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

(* Some useful basic fonctions *)
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
  let rec aux l =
    if exists p l then find p l
    else aux (flat_map r l)
  in
  aux [x]

let solve_path r p x =
  let r' = function
    | [] -> []
    | x::xs -> List.map (fun y -> y::x::xs) (r x) in
  let p' = function
    | [] -> false
    | x::_ -> p x in
  List.rev (solve r' p' [x])

let archive_map opset r (s, l) =
  let rec build_l = function
    | [] -> []
    | x::xs -> if opset.mem x s then build_l xs
      else x::(build_l xs) in
  let rec build_s = function
    | [] -> s
    | x::xs -> opset.add x (build_s xs) in
  let l' = build_l (flat_map r l) in
  (build_s l', l')

let solve' opset r p x =
  let rec aux (s, l) =
    if exists p l then find p l
    else aux (archive_map opset r (s, l))
  in
  aux (opset.empty, [x])

let solve_path' opset r p x =
  let r' = function
    | [] -> []
    | x::xs -> List.map (fun y -> y::x::xs) (r x) in
  let p' = function
    | [] -> false
    | x::_ -> p x in
  List.rev (solve' opset r' p' [x])

(* Example of puzzle:
 * Test the code with "Mister Rabbit's Great Escape"
 * Rabbit must goes from A to B.
 * He goes on straight line:  ---A------B--- or ---B----A---
 * He goes slowly, one step by one step forward. But he can
 * be afraid sometimes and go backwards by three steps. And
 * sometimes he becomes crazy and run so fast that he double
 * his position forward.
 * Example: A=17, B=15. To go from A to B one solution is
 *    1 - Go one step forward (reach 18)
 *    2 - Be afraid and jump three steps backward (reach 15)
 *
 * To fix ideas definitions can be
 *   type rabbit_move = Forward | Backward | Jump
 *   let rabbit_moves _ =  [ Forward ; Backward ; Jump ]
 *   let move_rabbit pos = function
 *     | Forward -> pos + 1
 *     | Backward -> pos - 3
 *     | Jump -> pos * 2
 *)

let solve_puzzle puz opset c =
  let all_confs c = List.map (fun m -> puz.move c m) (puz.possible_moves c) in
  solve_path' opset all_confs puz.final c

(* --- Part B: A Solver for Klotski --- *)
(*
 * Board coords are as follow
  [|
    [| (0,1); (0,2); (0,3); (0,4) |];
    [| (1,1); (1,2); (1,3); (1,4) |];
    [| (2,1); (2,2); (2,3); (2,4) |];
    [| (3,1); (3,2); (3,3); (3,4) |];
    [| (4,1); (4,2); (4,3); (4,4) |];
  |]
*)

let get_piece board x y =
  try
    Some (Array.get (Array.get board x) y)
  with _ -> None

let set_piece board piece x y =
    let row = Array.get board x in
    Array.set row y piece

let final board =
  let p1 = get_piece board 3 1 in
  let p2 = get_piece board 3 2 in
  let p3 = get_piece board 4 1 in
  let p4 = get_piece board 4 2 in
  let s = Some (S, 0) in
  p1 = s && p2 = s && p3 = s && p4 = s

let find_piece_in_row row piece =
  let row_size = Array.length row in
  let rec aux idx =
    if idx >= row_size then None
    else
      if piece = (Array.get row idx) then Some idx
      else aux (idx + 1) in
  aux 0;;

let find_piece_in_board board piece =
  let nb_row = Array.length board in
  let rec aux idx =
  if idx >= nb_row then None
  else
    match find_piece_in_row (Array.get board idx) piece with
    | None -> aux (idx + 1)
    | Some v -> Some (idx, v)
  in
  aux 0;;

let move_carre board piece { drow; dcol } =
  let try_to_move x y =
    match get_piece board (x + drow) (y + dcol) with
    | Some (X, _) -> (
        set_piece board piece (x + drow) (y + dcol);
        set_piece board (X, 0) x y;
        Some board
      )
    | _ -> None in
  match find_piece_in_board board piece with
  | None -> None
  | Some (x, y) -> try_to_move x y

(* The three following function needs to be implemented *)
let move_square = move_carre

let move_vrect = move_carre

let move_hrect = move_carre

let move_piece board piece {drow ; dcol} =
  match piece with
  | (S, _) -> move_square board piece {drow ; dcol}
  | (V, _) -> move_vrect board piece {drow ; dcol}
  | (H, _) -> move_hrect board piece {drow ; dcol}
  | (C, _) -> move_carre board piece {drow ; dcol}
  | _ -> None

exception WTF

let possible_moves board =
  let up    = {dcol = 0;  drow = -1} in
  let down  = {dcol = 0;  drow = 1} in
  let left  = {dcol = -1; drow = 0} in
  let right = {dcol = 1;  drow = 0} in
  let f = function
    | (_, _, None) -> false
    | _ -> true in
  let g = function
    | (a, b, Some c) -> Move (a, b, c)
    | _ -> raise WTF in
  let rec aux = function
    | [] -> []
    | x::xs -> (x, up, move_piece board x up) ::
               (x, down, move_piece board x down) ::
               (x, left, move_piece board x left) ::
               (x, right, move_piece board x right) :: aux xs in
  List.map g (List.filter f (aux all_pieces))

(*
 * TODO: Implement the following function

module BoardSet = Set.Make (struct
    type t = board
    let compare b1 b2 =
      failwith "Replace this with your implementation." ;;
  end)

let solve_klotski initial_board =
  "Replace this string with your implementation." ;;

*)
