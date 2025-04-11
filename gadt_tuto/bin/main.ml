let s_of_v = Gadt_tuto.Example.string_of_value

let () =
  let open Gadt_tuto.Example.Classical_variant in
  (* we want to add two integer *)
  let v1 = i 12 in
  let v2 = i 30 in
  plus_ v1 v2 |> eval |> s_of_v |> Printf.printf "[classical] %s\n";
  (* The issue is that I can write things like *)
  try
    let b1 = b true in
    plus_ v1 b1 |> eval |> s_of_v |> Printf.printf "[classical] %s\n"
    (* It compiles but it won't run *)
  with Gadt_tuto.Example.Ill_typed ->
    print_endline "[classical] ill typed detected at runtime"

let () =
  let open Gadt_tuto.Example.Phantom_variant in
  (* uncomment the block below and build. You will see that error
     is now detected at compile time .*)
  (* let _ = plus_ (i 12) (b true) in *)
  print_endline
    "[phantom] wrong usage of plus_ is detected at compile time now...";
  plus_ (i 12) (i 35) |> i_eval |> s_of_v |> Printf.printf "[phantom] %s\n";
  eq_ (b false) (b true) |> b_eval |> s_of_v |> Printf.printf "[phantom] %s\n"
