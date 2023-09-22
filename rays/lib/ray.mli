type t = { origin : Vec3.t; direction : Vec3.t }
(** A Ray has an origin and a direction *)

val create : o:Vec3.t -> d:Vec3.t -> t
(** [create ~o:origin ~d:direction] returns a ray *)

val direction : t -> Vec3.t
(** [direction r] returns the direction of the ray [r] *)

val origin : t -> Vec3.t
(** [origin r] returns the origin of the ray [r] *)

val at : t -> float -> Vec3.t
(** [at r t] It returns the 3D position of the ray [r] along its direction at
    time [t] *)

val raytrace : unit -> Image.t
(** [raytrace] creates an image, does the ray trace and return it *)
