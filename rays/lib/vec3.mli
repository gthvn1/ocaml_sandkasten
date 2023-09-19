type t = float * float * float

val orig : t

(* accessors *)
val x : t -> float
val y : t -> float
val z : t -> float

(* Operations *)
val length_squared : t -> float
val length : t -> float
val unit_vector : t -> t
val ( +. ) : t -> t -> t
val ( -. ) : t -> t -> t
val ( *. ) : float -> t -> t
val ( /. ) : t -> float -> t
val string_of_vec3 : t -> string
