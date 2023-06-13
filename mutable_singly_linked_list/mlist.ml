type 'a node = { value : 'a; mutable next : 'a node option }
(** ['a node] is a node of mutable singly linked list.
    It contains a value of type ['a] and an optionnaly has
    a pointer to the next node. *)

type 'a mlist = { mutable head : 'a node option }
(** ['a mlist] is a mutable singly linked list.
    It has an optional head that is the head of the list. *)

(** [create_node v] is a node containing value [v] with no link
    to another node. *)
let create_node v = { value = v; next = None }

(** [singleton v] is a singly linked list containing one value [v] *)
let singleton v = { head = Some (create_node v) }

(** [insert_first lst v] mutates [lst] by inserting value [v] as the
    first value in the list. *)
let insert_first lst v =
  match lst.head with
  | None -> lst.head <- Some (create_node v)
  | Some n ->
      let new_head = create_node v in
      new_head.next <- Some n;
      lst.head <- Some new_head
