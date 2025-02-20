(* define a function that takes no parameter *)
let revolt () = print_endline "Revolt!"

open Cmdliner

(* Now define a term for the function revolt *)
(* let revolt_t = Term.(const revolt $ const ()) *)
let revolt_t =
  let open Term in
  let term_revolt = const revolt in
  (* A term that evaluates to revolt *)
  let term_unit = const () in
  (* A term that evaluates to unit *)
  term_revolt $ term_unit (* Combines the two terms *)

(* Now we can attach the term to a command *)
let cmd = Cmd.v (Cmd.info "revolt") revolt_t

(* Finally we can evaluate the command *)
let () = exit (Cmd.eval cmd)
