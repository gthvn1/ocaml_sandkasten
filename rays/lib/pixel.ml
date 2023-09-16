type t = int * int * int

let white:t = (255, 255, 255)
let black:t = (0, 0, 0)

let create ~r ~g ~b =
  let is_invalid c = (c < 0) || (c > 255) in
  if (is_invalid r ) then failwith "Invalid red value";
  if (is_invalid g ) then failwith "Invalid green value";
  if (is_invalid b ) then failwith "Invalid blue value";
  (r, g, b)

let string_of_pixel (p:t) : string =
  let (r, g, b) = p in
  (string_of_int r) ^ " " ^
  (string_of_int g) ^ " " ^
  (string_of_int b)
