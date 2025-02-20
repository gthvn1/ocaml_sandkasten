(* [chorus count msg] print the [msg] [count] times *)
let chorus count msg =
  for _i = 1 to count do
    print_endline msg
  done

(*
   We want to make it as a command like: chorus [-c COUNT | --count=COUNT] [MSG]
 *)
open Cmdliner

(* Define the Term for arguments *)
let count =
  let doc = "Repeat the message $(docv) times." in
  Arg.(value & opt int 10 & info [ "c"; "count" ] ~docv:"COUNT" ~doc)

let msg =
  let env =
    let doc = "Overrides the default message to print." in
    Cmd.Env.info "CHORUS_MSG" ~doc
  in
  let doc = "The message to print." in
  Arg.(value & pos 0 string "Revolt!" & info [] ~env ~docv:"MSG" ~doc)

(* Define the term for executing chorus *)
let chorus_t = Term.(const chorus $ count $ msg)

let cmd =
  let doc = "print a customizable message repeatedly" in
  let man =
    [ `S Manpage.s_bugs; `P "Email bug reports to <bugs@example.org>." ]
  in
  let info = Cmd.info "chorus" ~version:"%%VERSION%%" ~doc ~man in
  Cmd.v info chorus_t

let main () = exit (Cmd.eval cmd)
let () = main ()
