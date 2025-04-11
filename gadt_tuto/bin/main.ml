let () =
  let open Gadt_tuto.Rpc_toy.Classical_variant in
  let s_of_v = Gadt_tuto.Rpc_toy.string_of_value in
  (* we want to add two integer *)
  let v1 = i 12 in
  let v2 = i 30 in
  plus_ v1 v2 |> eval |> s_of_v |> Printf.printf "res = %s\n";
  (* The issue is that I can write things like *)
  try
    let b1 = b true in
    plus_ v1 b1 |> eval |> s_of_v |> Printf.printf "res = %s\n"
  with Gadt_tuto.Rpc_toy.Ill_typed ->
    print_endline "ill typed detected at runtime"
