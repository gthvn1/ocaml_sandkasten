let read_file_by_two_chars filename =
  let ic = open_in filename in
  let next (c, ic) =
    try
      let carlu = input_char ic in
      Some ((c, carlu), (carlu, ic))
    with End_of_file ->
      close_in ic;
      None
  in
  match input_char ic with
  | exception End_of_file ->
      close_in ic;
      Seq.empty
  | first_char -> Seq.unfold next (first_char, ic)

(*
    Unfold constructs a sequence out of a step function and an initial state.
    If f u is None then unfold f u is the empty sequence.
    If f u is Some (x, u') then unfold f u is the nonempty sequence cons x (unfold f u').

    For example, unfold (function [] -> None | h :: t -> Some (h, t)) l is equivalent
    to List.to_seq l.

    *)

let () =
  let fname = "examples/threebytes.txt" in
  let f_seq = read_file_by_two_chars fname in

  let first_five_pairs = Seq.take 5 f_seq in
  Seq.iter (fun (c1, c2) -> Printf.printf "Read: %c%c\n" c1 c2) first_five_pairs
