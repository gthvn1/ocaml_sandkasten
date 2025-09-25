type cell = Wall | Floor

type t

val cell_size : int

val of_list : string list -> t

val width : t -> int

val height : t -> int

val iteri_cells : (int -> int -> cell -> unit) -> t -> unit

val is_floor : t -> x:int -> y:int -> bool
