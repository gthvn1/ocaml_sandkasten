external hello : string -> unit = "caml_hello"
external myadd: int -> int -> int = "caml_add"

let () =
    let res: int = myadd 40 2 in
    hello @@ string_of_int res
