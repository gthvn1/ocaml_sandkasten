let q1 () =
  let open Codekata_lib.Question1 in
  print_endline "1. Odd or Even Sum" ;
  ["1 2"; "8 2"; "12 31"]
  |> List.iter (fun s ->
         Printf.printf "  > %s : sum is %s\n" s (odd_or_even s) )

let () =
  print_endline "Starting CodeKata..." ;
  q1 ()
