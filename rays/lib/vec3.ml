type t = float * float * float

let orig : t = (0., 0., 0.)

(* accessors *)
let x ((x, _, _) : t) = x
let y ((_, y, _) : t) = y
let z ((_, _, z) : t) = z

(* Operations *)
let length_squared (v : t) : float =
  let x, y, z = v in
  (x *. x) +. (y *. y) +. (z *. z)

let length v = length_squared v |> sqrt

let unit_vector (v : t) : t =
  let l = length v in
  let x, y, z = v in
  (x /. l, y /. l, z /. l)

let ( +. ) (v1 : t) (v2 : t) : t =
  let x1, y1, z1 = v1 in
  let x2, y2, z2 = v2 in
  (x1 +. x2, y1 +. y2, z1 +. z2)

let ( -. ) (v1 : t) (v2 : t) : t =
  let x1, y1, z1 = v1 in
  let x2, y2, z2 = v2 in
  (x1 -. x2, y1 -. y2, z1 -. z2)

let ( *. ) a v =
  let x, y, z = v in
  (x *. a, y *. a, z *. a)

let ( /. ) v a =
  let x, y, z = v in
  (x /. a, y /. a, z /. a)

let string_of_vec3 v =
  let x, y, z = v in
  let sx = string_of_float x in
  let sy = string_of_float y in
  let sz = string_of_float z in
  "(" ^ sx ^ ", " ^ sy ^ ", " ^ sz ^ ")"
