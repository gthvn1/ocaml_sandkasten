type t = { r : float; g : float; b : float }
(** The pixel type

 A pixel is defined by its color using the RGB system. By convention red,
  green and blue are represented by float that range from 0.0 to 1.0.
 *)

val white : t
(** [white] is the pixel white *)

val black : t
(** [black] is the pixel black *)

val create : r:float -> g:float -> b:float -> t
(** [create ~r:r ~g:g ~b:b] creates a pixel where red is [r], green is [g] and
 red is [r]. It aborts if provided values are not in the range from 0.0 to 1.0 *)

val color : float -> int
(** [color c] scales float from range 0.0 to 1.0 to integer from range 0 to 255. *)

val string_of_pixel : t -> string
(** [string_of_pixel p] returns a representation of the pixel [p] as a string *)
