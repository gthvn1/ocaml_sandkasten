module Make (S : Storage_interface.T) = struct
  let create () = S.create ()
  let get db ~key = S.get db ~key
  let set db ~key ~value = S.set db ~key ~value
  let iter db ~fn = S.iter db ~fn
end
