type t = Pixel.t array array
(** An image is a matrix of pixels *)

val create : columns:int -> rows:int -> t
(** [create ~columns:c ~rows:r] creates an image of whidth [c] and height [r] *)

val get_rows : t -> int
(** [get_rows i] returns the number of rows of the image [i] *)

val get_columns : t -> int
(** [get_colmuns i] returns the number of columns of the image [i] *)

val transformation_example : t -> unit
(** [image_example i] takes an image and do some transformation. It is used for
    testing purposed. *)

val string_of_ppm : t -> string
(** [string_of_ppm i] returns the image [i] as a string using PPM format *)
