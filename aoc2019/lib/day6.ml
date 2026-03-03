(** [parse s] split strings [s] on char ')' and returns the two parts as a
    tuple. It raises an error if we don't have two parts. *)
let parse s =
  match String.split_on_char ')' s with
  | [ parent; child ] -> (child, parent)
  | _ -> failwith (Printf.sprintf "Failed to parse %s" s)

let input =
  [
    "COM)B"
  ; "B)C"
  ; "C)D"
  ; "D)E"
  ; "E)F"
  ; "B)G"
  ; "G)H"
  ; "D)I"
  ; "E)J"
  ; "J)K"
  ; "K)L"
  ]

module StringMap = Map.Make (String)

let map_of_orbits (edges : (string * string) list) : string StringMap.t =
  StringMap.of_list edges

let get_orbits map name =
  let rec aux orbits o =
    match StringMap.find_opt o map with
    | None -> orbits
    | Some parent -> aux (orbits + 1) parent
  in
  aux 0 name

let get_total_orbits map =
  let count = ref 0 in
  StringMap.iter (fun child _ -> count := !count + get_orbits map child) map;
  !count
