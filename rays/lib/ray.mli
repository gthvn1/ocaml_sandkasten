type t = { origin : Vec3.t; direction : Vec3.t }
(** A Ray has an origin and a direction *)

val create : o:Vec3.t -> d:Vec3.t -> t
(** [create ~o:origin ~d:direction] returns a ray *)

val direction : t -> Vec3.t
(** [direction t] returns the direction of the ray *)

val origin : t -> Vec3.t
(** [origin t] returns the origin of the ray *)
