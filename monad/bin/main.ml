(* A monad is a design that allows sequential composition of
   computations. It encapsulates a type and provides a way to chain
   operations.

   It consists of three things:
     1. A constructor M that wraps a value
     2. A return function that lifts a value into a monadic type
     3. A bind function that chains the computations
*)

(* Example with the "choix" Monad that is the option Monad *)

module Choix = struct
  (* 1. It needs a constructor *)
  type 'a t = Rien | Qqe of 'a

  (* 2. It needs a return function that will take 'a and transform
      it into a monadic type *)
  let return x = Qqe x

  (* 3. A bind function. So it is a function that takes a monadic
      type and apply the function to the lifted valued. *)
  let ( >>= ) mx f = match mx with Rien -> Rien | Qqe x -> f x
end

open Choix

(* [add_if_positive x y] takes to int and return a value if the result is positive.
   Otherwise it return Rien.
 *)
let add_if_positive x y =
  return x >>= fun a ->
  return y >>= fun b -> if a + b > 0 then Qqe (a + b) else Rien

(* [safe_div x y] returns the result of the division if [y] is not equal to zero. *)
let safe_div x y = if y <> 0 then Qqe (x / y) else Rien

(* [computation a b c] will divide a by b and add it to c if sum is positive *)
let computation a b c =
  safe_div a b >>= fun x ->
  (* if the monadic result of safe_div is Qqe then we take the value of this Qqe and apply the function.
     Note: it is the same for the previous [add_if_positive] but as [return x] always return Qqe the function
     is always applied. But it is the same semantic. *)
  return c >>= fun y -> add_if_positive x y

(*
  Let apply: add_if_positive 2 3

  -> return 2 >>= fun a ->
     return 3 >>= fun b ->
     if a + b > 0 then Qqe (a+b) else Rien

  -> return 2 is Some 2
  => Some 2 >>= fun a ->
     return 3 >>= fun b ->
     if a + b > 0 then Qqe (a+b) else Rien

  -> As it is Some 2 we can apply the binding
  => return 3 >>= fun b -> if 2 + b > 0 then Qqe (2 + b) else Rien

  -> Return 3 is Some 3
  => Some 3 >>= fun b -> if 2 + b > 0 then Qqe (2 + b) else Rien

  -> We apply the binding
  => if 2 + 3 > 0 then Qqe (2 + 3) else Rien => Qqe 5
 *)

let print_result = function
  | Rien -> print_endline "Computation aborted"
  | Qqe x ->
      print_int x;
      print_newline ()

let () =
  print_result @@ computation 4 0 6;
  print_result @@ computation 4 4 (-6);
  print_result @@ computation 4 2 6
