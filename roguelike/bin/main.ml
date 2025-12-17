let () =
  let term = Notty_unix.Term.create () in
  let x = ref 10 in
  let y = ref 10 in
  let rec loop () =
    (* get current terminal size to avoid hardcoding 80x24 *)
    let player = Notty.(I.char A.empty '@' 1 1) in
    let frame = Notty.I.pad ~l:!x ~t:!y player in
    Notty_unix.Term.image term frame ;
    match Notty_unix.Term.event term with
    | `Key (`Arrow `Up, []) ->
        decr y ; loop ()
    | `Key (`Arrow `Down, []) ->
        incr y ; loop ()
    | `Key (`Arrow `Left, []) ->
        decr x ; loop ()
    | `Key (`Arrow `Right, []) ->
        incr x ; loop ()
    | `Key (`Escape, []) | `Key (`ASCII 'q', []) ->
        ()
    | _ ->
        loop ()
  in
  loop ()
