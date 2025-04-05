(*
  We want to be able to evaluate, print, optimize or compile the following
  expression:

  -> (3 + 4) * 2
*)
type expr = Int of int | Add of expr * expr | Mul of expr * expr

(*
  Build (3 + 4) * 2
*)
let my_expr = Mul (Add (Int 3, Int 4), Int 2)

let rec eval exp =
  match exp with
  | Int x -> x
  | Add (a, b) -> eval a + eval b
  | Mul (a, b) -> eval a * eval b

(*
  This is a DSL because we have:
    - A syntax to construct arithmetic
    - A semantics to evaluate it
*)
