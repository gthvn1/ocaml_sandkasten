#!/usr/bin/env ocaml

let () =
  let build_res = Sys.command "dune build" in
  if build_res = 0 then (
    let run_res = Sys.command "./_build/default/effect_tuto.exe" in
    if run_res <> 0 then
      Printf.printf "Error: Failed to execute with code %d\n" run_res)
  else Printf.printf "Error: Build failed with code %d\n" build_res
