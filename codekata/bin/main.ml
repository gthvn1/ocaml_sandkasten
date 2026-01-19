let q2 () =
  print_endline "2. Prime Number Check";
  let open Codekata_lib.Question2 in
  Printf.printf "  > First 100 prime numbers: ";
  List.init 100 (fun x -> x)
  |> List.filter (fun x -> is_prime x)
  |> List.iter (fun x -> Printf.printf "%d " x);
  print_newline ()

let q1 () =
  let open Codekata_lib.Question1 in
  print_endline "1. Odd or Even Sum";
  [ "1 2"; "8 2"; "12 31" ]
  |> List.iter (fun s -> Printf.printf "  > %s : sum is %s\n" s (odd_or_even s))

let () =
  print_endline "Starting CodeKata...";
  q1 ();
  q2 ()
