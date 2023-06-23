let hello_dyn () = print_endline "Hello reload"

let () =
    Hello.hello := hello_dyn;
    print_endline "hello dyn loaded"
