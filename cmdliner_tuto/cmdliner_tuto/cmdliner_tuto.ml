(* Define a function that takes a name and a password as parameter *)
let greet user pass =
  Printf.printf "Hello, %s!\n" user;
  Printf.printf "Your password is %s..." pass

open Cmdliner

(* Define a term for the username argument *)
let username =
  let doc = "The name of the user." in
  Arg.(value & opt string "world" & info [ "name"; "n" ] ~doc)

(* Define a term for the password argument *)
let password =
  let doc = "The password of the user" in
  Arg.(value & opt string "changeme" & info [ "passwd"; "p" ] ~doc)

(* Define a term for the greet function *)
let greet_t = Term.(const greet $ username $ password)

let cmd =
  let doc = "A simple greeting program" in
  Cmd.v (Cmd.info "greet" ~doc) greet_t

(* Main entry point *)
let () = exit (Cmd.eval cmd)
