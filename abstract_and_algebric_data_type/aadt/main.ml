(* ******************
   Algebric data type
   ****************** *)

(* https://book.cs51.io/pdfs/abstraction.pdf *)

(* base is an algebric data type called "variant type" that uses
   "Alternation" (aka sum types) *)
(** DNA **)
type base = G | C | A | T

(* [comp_base bse] returns the complement of a base [bse]. *)
let comp_base = function A -> T | T -> A | G -> C | C -> G

(* dna is also an algebric data type that combines alternation (empty or not) and
   conjunction (product type) which joins base and dna *)
type dna = Nil | Cons of base * dna

let first_base = function
  | Nil -> failwith "empty DNA sequence"
  | Cons (x, _) -> x

(** Boolean document search **)

type document = { title : string; words : string list }

let tokenize str =
  let l = String.split_on_char '\n' str in
  let l = List.map (fun s -> String.split_on_char '\t' s) l |> List.concat in
  let l = List.map (fun s -> String.split_on_char ' ' s) l |> List.concat in
  List.map String.trim l

let first_lines : document list =
  (* output suppressed *)
  [
    { title = "Moby Dick"; words = tokenize "Call me Ishmael ." };
    {
      title = "Pride and Prejudice";
      words =
        tokenize
          "It is a truth universally acknowledged , that a single man in \
           possession of a good fortune must be in want of a wife .";
    };
    {
      title = "1984";
      words =
        tokenize
          "It was a bright cold day in April , and the clocks were striking \
           thirteen .";
    };
    {
      title = "Great Gatsby";
      words =
        tokenize
          "In my younger and more vulnerable years my father gave me some \
           advice that I've been turning over in my mind ever since .";
    };
  ]

let () =
  let dna_seq = Cons (C, Nil) in
  let _dna_seq = Cons (A, dna_seq) in
  print_endline "Bye, Sailor!"

(* *************************************
  Generatlized Algebric Data Type (GADT)
  ************************************** *)

(*
   expr ADT :
   - All variants share the same result (expr)
   *)

type expr = Int of int | Add of expr * expr | Mul of expr * expr

let rec eval expr =
  match expr with
  | Int n -> n
  | Add (e1, e2) -> eval e1 + eval e2
  | Mul (e1, e2) -> eval e1 * eval e2

let () =
  let e1 = Int 12 in
  let e2 = Int 32 in
  let e3 = Add (e1, e2) in
  Printf.printf "%d + %d = %d\n" (eval e1) (eval e2) (eval e3)

(* with GADT each variant can have its own result *)
type _ expr' =
  | Int : int -> int expr'
  | Add : int expr' * int expr' -> int expr'
  | Mul : int expr' * int expr' -> int expr'
  | Eq : int expr' * int expr' -> bool expr'

let rec eval' : type a. a expr' -> a = function
  | Int n -> n
  | Add (e1, e2) -> eval' e1 + eval' e2
  | Mul (e1, e2) -> eval' e1 * eval' e2
  | Eq (e1, e2) -> e1 = e2

let () =
  let e1 = Add (Int 1, Mul (Int 2, Int 3)) in
  let e2 = Add (Int 5, Mul (Int 2, Int 3)) in
  let e3 = Eq (e1, e2) in
  if eval' e3 then Printf.printf "%d is equal to %d\n" (eval' e1) (eval' e2)
  else Printf.printf "%d is not equal to %d\n" (eval' e1) (eval' e2)
