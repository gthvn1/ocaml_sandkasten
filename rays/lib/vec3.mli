type t = float * float * float
(** A vec3 is a 3D vector *)

val orig : t
(** [orig] is a vector where coordinate is {(0, 0, 0)} *)

val x : t -> float
(** [x v] returns the first coordinate of v that is x *)

val y : t -> float
(** [y v] returns the second coordinate of v that is y *)

val z : t -> float
(** [z v] returns the last coordinate of v that is z *)

val length : t -> float
(** [length v] returns the length of the vector [v] *)

val unit_vector : t -> t
(** [unit_vector v] returns the normed vector of [v] that is a vector of length 1 *)

val ( +++ ) : t -> t -> t
(** [v1 +++ v2] returns a new vector that is the addition of [v1] and [v2] *)

val ( --- ) : t -> t -> t
(** [v1 --- v2] returns a new vector that is the substraction of [v1] and [v2] *)

val ( *** ) : float -> t -> t
(** [a *** v] returns a new vector that is the multiplication of [v] by scalar [a] *)

val ( /// ) : t -> float -> t
(** [v /// a] returns a new vector that is the division of [v] by scalar [a] *)

val string_of_vec3 : t -> string
(** [string_of_vec3 v] returns the string that represents the vector [v] *)

val dot : t -> t -> float
(** [dot v1 v2] returns the dot product (aka scalar product) between [v1] and
    [v2] *)
