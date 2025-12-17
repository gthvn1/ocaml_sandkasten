(*
 https://erratique.ch/software/xmlm/doc/Xmlm/index.html
*)
let parse (input : Xmlm.input) =
  try
    let s = Xmlm.input input in
    match s with
    | `Dtd _ ->
        print_endline "found dtd"
    | `El_start _ ->
        print_endline "start tag"
    | `El_end ->
        print_endline "end tag"
    | `Data s ->
        print_endline s
  with _ -> print_endline "got error"

let parse_xml (filename : string) =
  let ic = open_in filename in
  Xmlm.make_input (`Channel ic) |> parse ;
  close_in ic

let () = parse_xml "sample.xml"
