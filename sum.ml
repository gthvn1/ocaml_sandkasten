(* To compile this code: # corebuild sum.native *)
open Core.Std

let rec read_and_accumulate accum =
    (* In_channel.input_line read a line from in_channel a return a string option *)
    let line = In_channel.input_line In_channel.stdin in
    match line with
    | None -> accum
    | Some x -> read_and_accumulate (accum +. Float.of_string x)

let () =
    printf "Total: %F\n" (read_and_accumulate 0.)
