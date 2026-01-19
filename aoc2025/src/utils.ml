module type Fname = sig
  val filename : string
end

module Openday (F : Fname) = struct
  let get_one_string () : string =
    let ic = open_in F.filename in
    let s = really_input_string ic (in_channel_length ic) in
    close_in ic;
    s

  let get_string_list () : string list =
    get_one_string () |> String.split_on_char '\n'
    |> List.filter (fun s -> String.length s > 0)
end
