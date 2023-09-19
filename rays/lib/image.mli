type t = Pixel.t array array
(** An image is a matrix of pixels *)

val create : columns:int -> rows:int -> t
val get_rows : t -> int
val get_columns : t -> int
val transform : t -> unit
val string_of_ppm : t -> string
