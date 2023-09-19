(*
* By convention red, green and blue are represented by float that range
* from 0.0 to 1.0.
*)
type t = { r : float; g : float; b : float }

let white : t = { r = 1.; g = 1.; b = 1. }
let black : t = { r = 0.; g = 0.; b = 0. }

let create ~r ~g ~b =
  let is_invalid v = v < 0.0 || v > 1.0 in
  if is_invalid r then failwith "Invalid red value";
  if is_invalid g then failwith "Invalid green value";
  if is_invalid b then failwith "Invalid blue value";
  { r; g; b }

let color (c : float) : int = c *. 255.999 |> int_of_float

let string_of_pixel (p : t) : string =
  let r = color p.r in
  let g = color p.g in
  let b = color p.b in
  string_of_int r ^ " " ^ string_of_int g ^ " " ^ string_of_int b
