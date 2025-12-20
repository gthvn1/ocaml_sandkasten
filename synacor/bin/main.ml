let () =
  let prog_filename =
    try Sys.argv.(1)
    with _ ->
      prerr_endline "USAGE: synacor <rom>" ;
      exit 1
  in
  let ic = open_in prog_filename in
  let rom = really_input_string ic (in_channel_length ic) in
  Emulator.run (Bytes.of_string rom) ~breakpoint:(Some 0x2)
