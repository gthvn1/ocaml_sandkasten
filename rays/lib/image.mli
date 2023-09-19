type t = Pixel.t array array
(** An image is a matrix of pixels *)

val create : columns:int -> rows:int -> t
(** [create ~columns:c ~rows:r] creates an image of whidth [c] and height [r] *)

val get_rows : t -> int
(** [get_rows i] returns the number of rows of the image [i] *)

val get_columns : t -> int
(** [get_colmuns i] returns the number of columns of the image [i] *)

val transform : t -> unit
(** [transform i] is a basic transformation use for testing purpose *)

val string_of_ppm : t -> string
(** [string_of_ppm i] returns the image [i] as a string using PPM format *)
