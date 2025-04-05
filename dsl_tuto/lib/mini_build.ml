type rule = { target : string; deps : string list; action : unit -> unit }

(*
  Let describes a rule to generate the object file of a hello.c
  and another one to generate the binary.
*)

let rules =
  [
    {
      target = "hello.o";
      deps = [ "hello.c" ];
      action = (fun () -> print_endline "gcc -c hello.c");
    };
    {
      target = "hello";
      deps = [ "hello.o" ];
      action = (fun () -> print_endline "gcc hello.o -o hello");
    };
  ]

let rec build target =
  match List.find_opt (fun r -> r.target = target) rules with
  | None -> Printf.printf "No rule to make target %s\n" target
  | Some r ->
      List.iter build r.deps;
      r.action ()

(*
  Again we have:
    - a syntax to construct rules
    - a build to evaluate them
 *)
