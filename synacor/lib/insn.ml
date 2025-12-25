type t =
  | Add of (int * int * int)
  | Eq of (int * int * int)
  | Halt
  | Jmp of int
  | Jt of (int * int)
  | Jf of (int * int)
  | Noop
  | Out of char
  | Set of (int * int)
  | Unknown

(** This is the memory area that contains the maximum number of words
    required by an instruction. The first integer is the opcode and it
    is required. Others are optional and their usage depends on the kind
    of the instruction. *)
type chunk = {opcode: int; ops: int option list}

let take_ops n (ops : int option list) : int list option =
  let rec aux lst acc i =
    if i = n then Some (List.rev acc)
    else
      match lst with
      | [] ->
          None
      | None :: _ ->
          None
      | Some v :: xs ->
          aux xs (v :: acc) (i + 1)
  in
  aux ops [] 0

(** Use a DSL to describe functions *)
type op_spec = {opcode: int; arity: int; build: int list -> t}

let op_specs : op_spec list =
  [ (* halt: 0, stop execution and terminate the program *)
    { opcode= 0
    ; arity= 0
    ; build= (fun lst -> match lst with [] -> Halt | _ -> assert false) }
  ; (* set: 1 a b,  set register <a> to the value of <b> *)
    { opcode= 1
    ; arity= 2
    ; build=
        (fun lst -> match lst with [a; b] -> Set (a, b) | _ -> assert false) }
  ; (* eq: 4 a b c,  set <a> to 1 if <b> is equal to <c>; set it to 0 otherwise *)
    { opcode= 4
    ; arity= 3
    ; build=
        (fun lst ->
          match lst with [a; b; c] -> Eq (a, b, c) | _ -> assert false ) }
  ; (* jmp: 6 a, jump to <a> *)
    { opcode= 6
    ; arity= 1
    ; build= (fun lst -> match lst with [a] -> Jmp a | _ -> assert false) }
  ; (* jt: 7 a b,  if <a> is nonzero, jump to <b> *)
    { opcode= 7
    ; arity= 2
    ; build=
        (fun lst -> match lst with [a; b] -> Jt (a, b) | _ -> assert false) }
  ; (* jf: 8 a b,  if <a> is zero, jump to <b> *)
    { opcode= 8
    ; arity= 2
    ; build=
        (fun lst -> match lst with [a; b] -> Jf (a, b) | _ -> assert false) }
  ; (* add: 9 a b c,  assign into <a> the sum of <b> and <c> (modulo 32768) *)
    { opcode= 9
    ; arity= 3
    ; build=
        (fun lst ->
          match lst with [a; b; c] -> Add (a, b, c) | _ -> assert false ) }
  ; (* out: 19 a,  write the character represented by ascii code <a> to the terminal *)
    { opcode= 19
    ; arity= 1
    ; build=
        (fun lst -> match lst with [a] -> Out (Char.chr a) | _ -> assert false)
    }
  ; (* noop: 21,  no operation *)
    { opcode= 21
    ; arity= 0
    ; build= (fun lst -> match lst with [] -> Noop | _ -> assert false) } ]

(** [decode_with_spec chunk] will go through the list of specification
    op_specs to find the correct opcode and return the instruction with
    the size of the instruction. It can be improved by using a HashMap but
    for our small program it is ok. *)
let decode_with_spec (spec : op_spec) (c : chunk) : (t * int) option =
  if spec.opcode <> c.opcode then None
  else
    match take_ops spec.arity c.ops with
    | None ->
        None
    | Some args ->
        Some (spec.build args, spec.arity + 1)

let decode (c : chunk) : (t * int) option =
  let rec loop = function
    | [] ->
        None
    | s :: xs -> (
      match decode_with_spec s c with None -> loop xs | Some _ as r -> r )
  in
  loop op_specs

let to_string (insn : t) : string =
  match insn with
  | Add (a, b, c) ->
      Printf.sprintf "ADD 0x%x 0x%x 0x%x" a b c
  | Eq (a, b, c) ->
      Printf.sprintf "EQ 0x%x 0x%x 0x%x" a b c
  | Halt ->
      "HALT"
  | Jmp addr ->
      Printf.sprintf "JUMP 0x%x" addr
  | Jt (value, addr) ->
      Printf.sprintf "JT %d 0x%x" value addr
  | Jf (value, addr) ->
      Printf.sprintf "JF %d 0x%x" value addr
  | Noop ->
      "NOOP"
  | Out c ->
      Printf.sprintf "OUT %c" c
  | Set (reg, addr) ->
      Printf.sprintf "SET %d 0x%x" reg addr
  | Unknown ->
      "UNKNOWN"
