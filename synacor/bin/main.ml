let () =
  (* Todo: get program from first arg *)
  let prog = Bytes.make 10 'a' in
  Emulator.run prog
