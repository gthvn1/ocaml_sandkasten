(*
* By convention red, green and blue are represented by float that range
* from 0.0 to 1.0.
*)
type t = { r : float; g : float; b : float }

val white : t
val black : t
val create : r:float -> g:float -> b:float -> t
val color : float -> int
val string_of_pixel : t -> string
