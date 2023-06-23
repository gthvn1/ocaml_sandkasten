let hello_original () = print_endline "Hello from original"

let hello: (unit -> unit) ref = ref hello_original
