type 'a t = (string, 'a) Hashtbl.t

let create () : 'a t = Hashtbl.create 1024
let get db ~(key : string) : 'a option = Hashtbl.find_opt db key
let set db ~(key : string) ~(value : 'a) = Hashtbl.add db key value
let iter db ~(fn : string -> 'a -> unit) = Hashtbl.iter fn db
