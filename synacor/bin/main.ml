type arguments = {break: int option; debug: bool; rom: string}

let get_args () : arguments =
  let args = Sys.argv in
  let args_len = Array.length args in
  let rec aux i b d r =
    if i >= args_len then (b, d, r)
    else
      match args.(i) with
      | "--break" ->
          if Option.is_some b then failwith "--break called twice" ;
          aux (i + 2) (Some (int_of_string args.(i + 1))) d r
      | "--debug" ->
          aux (i + 1) b true r
      | "--help" ->
          print_endline "USAGE: synacor [--break <addr>] [--debug] <rom>" ;
          exit 1
      | s ->
          if Option.is_some r then failwith "More than one rom selected" ;
          aux (i + 1) b d (Some s)
  in
  let break, debug, rom = aux 1 None false None in
  match rom with
  | None ->
      failwith "USAGE: synacor [--break <addr>] [--debug] <rom>"
  | Some r ->
      {break; debug; rom= r}

let () =
  let args = get_args () in
  let ic = open_in args.rom in
  let rom = really_input_string ic (in_channel_length ic) in
  Emulator.run ~breakpoint:args.break ~debug_mode:args.debug
    (Bytes.of_string rom)
