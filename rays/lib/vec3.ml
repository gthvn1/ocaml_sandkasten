type t = float * float * float

(* accessors *)
let x (v : t) =
  let x1, _, _ = v in
  x1

let y (v : t) =
  let _, y1, _ = v in
  y1

let z (v : t) =
  let _, _, z1 = v in
  z1

let ( +. ) (v1 : t) (v2 : t) : t =
  let x1, y1, z1 = v1 in
  let x2, y2, z2 = v2 in
  (x1 +. x2, y1 +. y2, z1 +. z2)

let ( *. ) v a =
  let x1, y1, z1 = v in
  (x1 *. a, y1 *. a, z1 *. a)
