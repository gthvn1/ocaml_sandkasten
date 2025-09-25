type cell = Wall | Floor

type t = cell array array

let cell_size : int = 20
(* in pixels *)

(** [of_string str] converts a string [str] into an array of cells. *)
let of_string (str : string) : cell array =
  str |> String.to_seq
  |> Seq.map (fun c -> if c = '#' then Wall else Floor)
  |> Array.of_seq

(** [of_file filename] returns a list of string read from [filename]. *)
let of_list (lst : string list) : t = lst |> List.map of_string |> Array.of_list

let iteri_cells f (board : t) : unit =
  Array.iteri (fun y row -> Array.iteri (fun x cell -> f x y cell) row) board

let width (board : t) : int = Array.length board.(0)

let height (board : t) : int = Array.length board

let is_floor (board : t) ~(x : int) ~(y : int) : bool =
  try board.(y).(x) = Floor with Invalid_argument _ -> false
