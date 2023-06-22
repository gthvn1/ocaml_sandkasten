module type TableMap = sig

  (** [('k, 'v) t] is the type of mutable table-based maps that
      bind keys of type ['k] to values of type ['v]. *)
  type ('k, 'v) t

  (** [insert k v m] mutates map [m] to bind [k] to [v].
      If [k] is already bound in [m], that binding is replaced
      by the new binding to [v]. *)
  val insert: 'k -> 'v -> ('k, 'v) t -> unit

  (** [find k m] is [Some v] if [m] binds [k] to [v], and
      [None] if not. *)
  val find: 'k -> ('k, 'v) t -> 'v option

  (** [remove k m] mutates [m] to remove any binding of [k].
      If [k] is not bound in [m] the map is unchanged. *)
  val remove: 'k -> ('k, 'v) t -> unit

  (** [create hash c] creates a new table map with capacity [c]
      that will use [hash] as the function to convert keys to
      integers.
      Requires: [hash] distributes keys uniformaly. *)
  val create : ('k -> int) -> int -> ('k, 'v) t
end

module HashMap : TableMap = struct

  type ('k, 'v) t = {
    hash: 'k -> int;
    mutable size: int;
    buckets : ('k * 'v) list array
  }

  let bucket_from_key k m =
    let b = m.hash k in
    b mod (Array.length m.buckets)
    
  let insert_pair k v m = 
      let b = bucket_from_key k m in
      let () = Printf.printf  "b = %d\n" b in
      let old_bucket = m.buckets.(b) in
      m.buckets.(b) <- (k, v) :: (List.remove_assoc k old_bucket);
      match List.assoc_opt k old_bucket with
      | None -> m.size <- m.size + 1
      | _ -> () (*the key was already here so don't count it *)

  let insert k v m =
      insert_pair k v m

  let find k m = 
      let b = bucket_from_key k m in
      List.assoc_opt k m.buckets.(b)

  let remove k m =
      let b = bucket_from_key k m in
      let old_bucket = m.buckets.(b) in
      m.buckets.(b) <- List.remove_assoc k m.buckets.(b);
      match List.assoc_opt k old_bucket with
      | Some(_) -> m.size <- m.size - 1
      | None -> () (*the key was already here so don't count it *)

  let create h c = {
    hash = h;
    size = 0;                 (* there is no bindings for now *)
    buckets = Array.make c [] (* all buckets are empty *)
  }
  
end