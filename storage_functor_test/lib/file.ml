module MemStorage = Storage.Make (Hashmem_storage)

let execute () =
  let db = MemStorage.create () in
  MemStorage.set db ~key:"hello" ~value:"world";
  MemStorage.set db ~key:"foo" ~value:"bar";
  MemStorage.iter db ~fn:(fun k v -> Printf.printf "Key: %s, Value: %s\n" k v)
