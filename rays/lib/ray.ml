type t = { origin : Vec3.t; direction : Vec3.t }

let create ~(o : Vec3.t) ~(d : Vec3.t) : t = { origin = o; direction = d }
let direction (r : t) = r.direction
let origin (r : t) = r.origin
