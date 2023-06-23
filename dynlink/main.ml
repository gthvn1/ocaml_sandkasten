let reload_lib () : unit =
    let open Dynlink in
    loadfile_private "hello_dyn.cmo";
    Printf.printf "Lib reloaded\n"

let () =
    let rec loop () =
        Printf.printf "Enter something (R to reload, Q to quit, H to print hello): ";
        match read_line () with
        | "q" | "Q" -> Printf.printf "Bye\n"; Stdlib.exit 0
        | "r" | "R" -> Printf.printf "Reload\n"; reload_lib (); loop ()
        | "h" | "H" -> !Hello.hello (); loop ()
        | input -> Printf.printf "echo: %s\n" input; loop ()
    in loop ()
