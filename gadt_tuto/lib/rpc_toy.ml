(* Start with https://dev.realworldocaml.org/gadts.html *)

(*
 Try to build language that mix arithmetic and boolean.
 *)

(** We want to be able to use booleans and integers. *)
type value = Int of int | Bool of bool

(** [string_of_value v] returns a string from the value [v] *)
let string_of_value v =
  match v with
  | Int x -> Printf.sprintf "%d" x
  | Bool x -> Printf.sprintf "%s" (if x then "true" else "false")

(** And we want to be able to add integers, to compare booleans and also do an
    If cond then exp1 else expr2. Let's start with classical variant: *)

(* As we cannot add two booleans we can have ill typed exception. *)
exception Ill_typed

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

module Classical_variant : Classical_variant_sig = struct
  type expr =
    | Value of value
    | Eq of expr * expr
    | Plus of expr * expr
    | If of expr * expr * expr

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
  let rec eval (e : t) =
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
end

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

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
