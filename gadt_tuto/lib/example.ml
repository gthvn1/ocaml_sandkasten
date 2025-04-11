(* Start with https://dev.realworldocaml.org/gadts.html *)

(*
 Try to build language that mix arithmetic and boolean.
 *)

(** We want to be able to use booleans and integers. *)
type value = Int of int | Bool of bool

type expr =
  | Value of value
  | Eq of expr * expr
  | Plus of expr * expr
  | If of expr * expr * expr

(** [string_of_value v] returns a string from the value [v] *)
let string_of_value v =
  match v with
  | Int x -> Printf.sprintf "%d" x
  | Bool x -> Printf.sprintf "%s" (if x then "true" else "false")

(** And we want to be able to add integers, to compare booleans and also do an
    If cond then exp1 else expr2. Let's start with classical variant: *)

(* As we cannot add two booleans we can have ill typed exception. *)
exception Ill_typed

(* -------------------------------------------------------------------------- *)
(* CLASSICAL VARIANT                                                          *)
(* -------------------------------------------------------------------------- *)
module type Classical_variant_sig = sig
  type t

  (* We need some constructors *)
  val i : int -> t
  val b : bool -> t
  val plus_ : t -> t -> t
  val eq_ : t -> t -> t
  val if_ : t -> t -> t -> t

  (* We need an evaluator *)
  val eval : t -> value
end

let rec eval (e : expr) : value =
  match e with
  | Value v -> v
  | Eq (e1, e2) -> (
      match (eval e1, eval e2) with
      | Bool x, Bool y -> Bool (x = y)
      | Int x, Int y -> Bool (x = y)
      | _ -> raise Ill_typed)
  | Plus (e1, e2) -> (
      match (eval e1, eval e2) with
      | Int x, Int y -> Int (x + y)
      | _ -> raise Ill_typed)
  | If (c1, e1, e2) -> (
      match eval c1 with
      | Bool b -> if b then eval e1 else eval e2
      | _ -> raise Ill_typed)

module Classical_variant : Classical_variant_sig = struct
  type t = expr

  (* Constructors *)
  let i x = Value (Int x)
  let b x = Value (Bool x)
  let plus_ e1 e2 = Plus (e1, e2)
  let eq_ e1 e2 = Eq (e1, e2)
  let if_ c1 e1 e2 = If (c1, e1, e2)

  (** Evaluator: with ADT and variants we see that we check the type during the
      execution. IT is because when we use a classical ADT we can only have one
      type as output that is "expr" *)
  let eval = eval
end

(* -------------------------------------------------------------------------- *)
(* PHANTOM VARIANT                                                            *)
(* -------------------------------------------------------------------------- *)
(* To allow the type check at compile time we need a way to store the information within
the type. For this we can start by using phantom type. *)
module type Phantom_variant_sig = sig
  type 'a t
  (* Phantom type allow to use another type and it is called phantom because we won't use
     it in the right part of the definition. We will see that in the implementation.
     As it is not used we can also write: type _ t *)

  (* We need some constructors: we see that now the information of the type is
      indicated in the phantom type *)
  val i : int -> int t
  val b : bool -> bool t
  val plus_ : int t -> int t -> int t
  val eq_ : 'a t -> 'a t -> bool t
  val if_ : bool t -> int t -> int t -> int t

  (* We need an evaluator. In fact now that we want to be able to differenciate integer and boolean
  at compile time we need one evalutar per type *)
  val i_eval : int t -> value
  val b_eval : bool t -> value
end

module Phantom_variant : Phantom_variant_sig = struct
  type _ t = expr
  (* here we see why it is a phantom type, the generalized part is not used in the right side *)

  let i x = Value (Int x)
  let b x = Value (Bool x)
  let plus_ x y = Plus (x, y)
  let eq_ a b = Eq (a, b)
  let if_ c x y = If (c, x, y)

  (* We can reuse the classical variant even if we now know that checks are done at compile time *)
  let i_eval = eval
  let b_eval = eval
end

(* -------------------------------------------------------------------------- *)
(* GADT: here we are                                                          *)
(* -------------------------------------------------------------------------- *)

type _ value' =
  | Int : int -> int value'
  | Bool : bool -> bool value'
      (** The ':' shows that it is a GADT

          -> To the right of the colon with something that looks like single
          argument function type. You can think of it has the type signature for
          the tag, viewed as a type constructor. *)

type _ expr' =
  | Value : 'a value' -> 'a expr'
  | Eq : 'a expr' * 'a expr' -> bool expr'
  | Plus : int expr' * int expr' -> int expr'
  | If : bool expr' * 'a expr' * 'a expr' -> 'a expr'

let i' x = Value (Int x)
let b' x = Value (Bool x)
let plus' x y = Plus (x, y)
let eq' x y = Eq (x, y)
let if' c x y = If (c, x, y)

(*
   Here like with phantom type we have the type safety rules

     🐫 > Plus ((Value (Int 12)), (Value (Int 12)));;
     - : int expr' = Plus (Value (Int 12), Value (Int 12))
     🐫 > Plus ((Value (Int 12)), (Value (Bool false)));;
     Error: This constructor has type bool value'
            but an expression was expected of type int value'
            Type bool is not compatible with type int
*)

(* But now we can have on eval function... *)
let rec eval' : type a. a expr' -> a = function
  | Value v -> ( match v with Int x -> x | Bool x -> x)
  | Eq (e1, e2) -> eval' e1 = eval' e2
  | Plus (i1, i2) -> eval' i1 + eval' i2
  | If (c1, e1, e2) -> if eval' c1 then eval' e1 else eval' e2

(*
  It looks pretty neat.
  To achieve this we need to use a locally abstract type.
*)

(* -------------------------------------------------------------------------- *)
(* When Are GADTs Useful?                                                     *)
(* -------------------------------------------------------------------------- *)

(** We want to create a version of kind that can handle issue in different
    maners

    - It can thrown an exception
    - Return None
    - Return a default value *)
module If_not_found = struct
  type 'a t = Raise | Return_none | Default_to of 'a
end

(* Let's write our find function *)
let rec flexible_find list ~f (if_not_found : _ If_not_found.t) =
  match list with
  | hd :: tl -> if f hd then Some hd else flexible_find tl ~f if_not_found
  | [] -> (
      match if_not_found with
      | Raise -> failwith "Element not found"
      | Return_none -> None
      | Default_to x -> Some x)

(* The problem here is that if we know that we will return a default value it could better
   to return the value and not the option. In this case where we have different return values
   we can use GADT. *)

module If_not_found' = struct
  type (_, _) t =
    | Raise : ('a, 'a) t
    | Return_none : ('a, 'a option) t
    | Default_to : 'a -> ('a, 'a) t
end

(* What is important here is that we see that we have a list of elements of type 'a and
  the return type can be either of the same type of the elements of the list (so type 'a) but
  it can also be an Option. So we have two types. We can see it in the signature of If_not_found' *)
let rec flexible_find' : type a b.
    a list -> f:(a -> bool) -> (a, b) If_not_found'.t -> b =
 fun list ~f if_not_found ->
  match list with
  | [] -> (
      match if_not_found with
      | Raise -> failwith "No matching item found"
      | Return_none -> None
      | Default_to x -> x)
  | hd :: tl ->
      if f hd then
        match if_not_found with
        | Raise -> hd
        | Return_none -> Some hd
        | Default_to _ -> hd
      else flexible_find' tl ~f if_not_found

(* 

(* Type representing an RPC call *)
type call = { name : string; args : (string * string) list }

(* Type representing an RPC response *)
type response = { success : bool; result : string }

(* Dummy marshalling *)
let marshal v = v
let unmarshal v = v

(* Dummy RPC transport *)
let rpc call =
  Printf.printf "Calling RPC: %s with args: %s\n"
    call.name
    (String.concat ", "
       (List.map (fun (k, v) -> k ^ "=" ^ v) call.args));
  { success = true; result = "42" }

(* DSL for function signatures using a GADT *)
type _ fn =
  | Ret : 'a -> 'a fn
  | Arg : string * ('a -> 'b fn) -> ('a -> 'b) fn


(* 'declare' turns a DSL into a real OCaml function *)
let rec declare : type a. string -> a fn -> a =
  fun name -> fun ty ->
    match ty with
    | Ret result ->
        (* End of chain: build and call the rpc *)
        fun () ->
          let call = { name; args = [] } in
          let response = rpc call in
          unmarshal response.result
    | Arg (param_name, next) ->
        fun v ->
          let v = marshal v in
          let f = declare param_name (next v) in
          (* Build the argument list — simplified here *)
          (* In a real system you'd pass args down recursively *)
          f

(* Example: declare an RPC "add" that takes two ints and returns an int *)
let add_rpc =
  declare "add"
    (Arg ("x", fun x ->
     Arg ("y", fun y ->
     Ret (x + y))))  (* In real RPC, this would be response-unmarshalling *)

*)
