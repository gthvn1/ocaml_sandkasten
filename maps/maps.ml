module type Map = sig
  type ('k, 'v) t
  (** [t] is the type of the maps that bind keys of type ['k]
        to values of type ['v]. *)

  val insert : 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t
  (** [insert k v m] is the same map as [m], but with an additional binding
        from [k] to [v]. If [k] was already bound in [m], that binding is
        replaced by the binding to [v] in the new map. *)

  val find : 'k -> ('k, 'v) t -> 'v option
  (** [find k m] is [Some v] if [k] is bound to [v] in [m] and None if not. *)

  val remove : 'k -> ('k, 'v) t -> ('k, 'v) t
  (** [remove k m] is the same map as [m], but without any binding of [k].
        If [k] was not bound in [m], then the map is unchanged. *)

  val empty : ('k, 'v) t
  (** [empty] is the empty map. *)

  val of_list : ('k * 'v) list -> ('k, 'v) t
  (** [of_list lst] is a map containing the same bindings as association
        list [lst].
        Requires: [lst] does not contain any duplicate keys. *)

  val bindings : ('k, 'v) t -> ('k * 'v) list
  (** [bindings m] is an association list containing the same bindings as
        [m]. There are no duplicate keys in the list. *)
end

module AssocListMap : Map = struct
  (**
 What is the representation type ?

 What is the abstraction function ?
 AF =>
    [[(k1, v1); (k2, v2); ... ; (kn, vn)]] is the map
    {k1: v1, k2: v2, ..., kn: vn}. If a key appears more than once in the list,
    then in the map it is bound to the left-most occurence in the list.
    For example, [[(k, v1); (k, v2)]] represents the map {k: v1}.
 
 What are the representation invariants?
 RI => None
 
 What is the efficiency of each operation?
 *)

  type ('k, 'v) t = ('k * 'v) list

  let insert _ _ _ = failwith "insert unimplemented"
  let find _ _ = failwith "find unimplemented"
  let remove _ _ = failwith "remove unimplemented"
  let empty = []
  let of_list _ = failwith "of_list unimplemented"
  let bindings m = m
end
