(*
   What we want is a map where for a given output chemical we can
   get the input chemical.
   For example
     - "9 ORE => 2 A" should produce reaction["2 A"] -> "9 ORE"
     - "4 C, 1 A => 1 CA" -> reaction["1 CA"] -> ["4 C"; "1 A"]

   But instead of using string we can use chemical type.
*)

let sample_input =
  [
    "9 ORE => 2 A"
  ; "8 ORE => 3 B"
  ; "7 ORE => 5 C"
  ; "3 A, 4 B => 1 AB"
  ; "5 B, 7 C => 1 BC"
  ; "4 C, 1 A => 1 CA"
  ; "2 AB, 3 BC, 4 CA => 1 FUEL"
  ]

(*
   For input we want to produce a map. The entry is a symbol as a string.
   The value is the quantity produced with the list of inputs.
 *)
type chemical = { symbol : string; quantity : int }
type reaction = { symbol : string; quantity : int; inputs : chemical list }

module ChemicalMap = Map.Make (String)

(** [chemical_of_string s] returns a chemical element from [s].

    @raise Failure in case of error. *)
let chemical_of_string s : chemical =
  match String.split_on_char ' ' s with
  | [ q; s ] -> { symbol = String.trim s; quantity = int_of_string q }
  | _ -> failwith "Failed to extract chemical from string"

(** [chemicals_of_inputs i] extracts a list of chemical from [i].
    @raise Failure. *)
let chemicals_of_inputs i =
  String.split_on_char ',' i |> List.map String.trim
  |> List.map chemical_of_string

(** [reaction_of_string s] returns the reaction parsed from [s]. Example:
    {[
      reaction_of_string "9 ORE => 2 A"
      = {
          symbol = "A"
        ; quantity = 2
        ; inputs = [ { symbol = "ORE"; quantity = 9 } ]
        }
    ]}

    @raise Failure if the reaction cannot be parsed. *)
let reaction_of_string s =
  let eq_idx = String.index s '=' in
  let inputs = String.(sub s 0 eq_idx |> trim) in
  let eq_idx = eq_idx + 2 in
  (* skip '=' and '>' *)
  let output = String.(sub s eq_idx (length s - eq_idx) |> trim) in
  let quantity, symbol =
    match String.split_on_char ' ' output with
    | [ q; s ] -> (int_of_string q, String.trim s)
    | _ -> failwith "Failed to separate quantity and symbol"
  in
  { symbol; quantity; inputs = chemicals_of_inputs inputs }

(** [add_reaction map symbol quantity inputs] adds into [map] the [quantity] of
    [symbol] produced by the [inputs].
    @raise Failure if the symbol is alread in the map. *)
let add_reaction ~map reaction =
  if ChemicalMap.mem reaction.symbol map then
    failwith (Printf.sprintf "Symbol [%s] already set" reaction.symbol);
  ChemicalMap.add reaction.symbol (reaction.quantity, reaction.inputs) map

(** [fill_reactions inputs] build a map of reactions from a list of string
    reaction.
    @raise Failure. *)
let fill_reactions (inputs : string list) =
  List.fold_left
    (fun m s -> add_reaction ~map:m (reaction_of_string s))
    ChemicalMap.empty inputs

(** [get_inputs ~map ~quantity ~symbol] retuns the list of inputs required to
    produce [quantity] of [symbol]. As a reaction can produce more than expected
    it also returned the extra produced element.

    @raise an exception if symbol cannot be produced.

    Example considering the map is filled with sample_input.
    - get_inputs ~map ~quantity:1 ~symbol:"FUEL" -> Some (
      [{symbol="AB"; quantity=2}; {symbol="BC"; quantity=3}; {symbol="CA";
       quantity=4}] , [])
    - get_inputs ~map ~quantity:2 ~symbol:"AB" -> Some (
      [{symbol="A"; quantity=6}; {symbol="B"; quantity=8}] , [])
    - get_inputs ~map ~quantity:4 ~symbol:"C" -> Some (
      [{symbol = "ORE"; quantity = 7}] , [{symbol = "C"; quantity = 1]) *)
let get_inputs ~map ~quantity ~symbol =
  let qty_produced, inputs = ChemicalMap.find symbol map in
  let batches =
    (quantity / qty_produced) + if quantity mod qty_produced = 0 then 0 else 1
  in
  let inputs : chemical list =
    List.map
      (fun ({ symbol; quantity = q } : chemical) ->
        { symbol; quantity = batches * q })
      inputs
  in
  let extra = (batches * qty_produced) - quantity in
  (inputs, if extra = 0 then [] else [ { symbol; quantity = extra } ])

type state = { wanted : chemical list; required : int; extra : chemical list }

(** [use_extra c e] attempts to satisfy the requirement [c] using surplus
    chemicals from [e].

    If the surplus fully satisfies [c], it returns [(None, e')] where [e'] is
    the updated list of remaining surplus chemicals.

    Otherwise it returns [(Some c', e')] where [c'] is the remaining quantity
    still required and [e'] is the updated surplus list. *)
let use_extra (ch : chemical) (e : chemical list) :
    chemical option * chemical list =
  let rec aux (c : chemical) (keep : chemical list)
      (not_processed : chemical list) =
    match not_processed with
    | [] -> ((if c.quantity = 0 then None else Some c), keep)
    | c' :: xs as l ->
        if c.quantity = 0 then (None, keep @ l)
        else if c.symbol = c'.symbol then
          match c.quantity - c'.quantity with
          | 0 -> (None, keep @ xs)
          | qty when qty > 0 -> aux { c with quantity = qty } keep xs
          | qty -> (None, { c' with quantity = -qty } :: (keep @ xs))
        else aux c (c' :: keep) xs
  in
  aux ch [] e

(** [one_step ~map ~state] performs one expansion step on [state].

    If [state.wanted] is empty, the state is returned unchanged.

    Otherwise, the first chemical in [wanted] is expanded using [map]. The
    required inputs to produce it are computed via [get_inputs].

    - Any required ORE is accumulated into [state.required].
    - Non-ORE inputs are appended to [state.wanted].
    - Any surplus production is added to [state.extra].

    The updated state is then returned. *)
let one_step ~map ({ wanted; required; extra } : state) : state =
  match wanted with
  | [] -> { wanted; required; extra }
  | c :: cs -> (
      (* Before getting inputs try to use [extra] chemicals *)
      match use_extra c extra with
      | None, e -> { wanted = cs; required; extra = e }
      | Some c, e ->
          let needed, leftover =
            get_inputs ~map ~quantity:c.quantity ~symbol:c.symbol
          in
          let rec filter_ore (acc : chemical list) (ore : int)
              (l : chemical list) =
            match l with
            | [] -> (acc, ore)
            | { symbol = "ORE"; quantity = q } :: xs ->
                filter_ore acc (ore + q) xs
            | c :: xs -> filter_ore (c :: acc) ore xs
          in
          let a, o = filter_ore [] 0 needed in
          { wanted = a @ cs; required = required + o; extra = e @ leftover })

let init_state : state =
  { wanted = [ { symbol = "FUEL"; quantity = 1 } ]; required = 0; extra = [] }
