(* A monad is a design that allows sequential composition of
   computations. It encapsulates a type and provides a way to chain
   operations.

   It consists of three things:
     1. A constructor M that wraps a value
     2. A return function that lifts a value into a monadic type
     3. A bind function that chains the computations

  Links: https://cs3110.github.io/textbook/chapters/ds/monads.html
*)

(* Example with the "choix" Monad that is the option Monad

  Notes (it is my understanding...):
    - the type in Choix is called a covariant functor
    - adding the monadic operations (return, bind) to this covariant
    functor produces a free monadic structure.
    - We can also use functor (in the OCaml sense) to create a new module
    and add monadic operations to this functor. In this case it is a
    monadic structure and not a free monadic structure.
      - See the end of the file for an example:
 *)

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

let inc x = x + 1
let dec x = x - 1

(*let inc_log x = (x + 1, Printf.sprintf "Called inc on %i; " x)*)
(*let dec_log x = (x - 1, Printf.sprintf "Called dec on %i; " x)*)

(* We can defined the function id by using inc and dec for example*)
let id x = x |> inc |> dec

(* But we cannot do it using inc_log. If we want to do it we need to update
  the inc_log to take a tuple (int , string) as a parameter *)
let inc_log' (x, s) = (x + 1, s ^ Printf.sprintf "Called inc on %i; " x)
let dec_log' (x, s) = (x - 1, s ^ Printf.sprintf "Called dec on %i; " x)

(* We want to log string when executing the function *)
let log (name : string) (f : int -> int) : int -> int * string =
 fun x -> (f x, Printf.sprintf "Called %s on %i; " name x)

let loggable (name : string) (f : int -> int) : int * string -> int * string =
 fun (x, s1) ->
  let y, s2 = log name f x in
  (y, s1 ^ s2)

let print_result = function
  | Rien -> print_endline "Computation aborted"
  | Qqe x ->
      print_int x;
      print_newline ()

let () =
  print_result @@ computation 4 0 6;
  print_result @@ computation 4 4 (-6);
  print_result @@ computation 4 2 6

(*
     Monadic structure from functor

      module type FUNCTOR = sig
        type 'a t
        val map : 'a t -> ('a -> 'b) -> 'b t
      end

      module Free (F : FUNCTOR) = struct
        type 'a t =
          | Return of 'a
          | Bind of 'a F.t * ('a -> 'b t)

        let return x = Return x
        let bind m f = match m with
          | Return x -> f x
          | Bind (x, g) -> Bind (x, fun a -> bind (g a) f)
      end

   *)
