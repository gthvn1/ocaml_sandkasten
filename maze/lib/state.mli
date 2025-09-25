type t

val is_box_at : t -> pos:Pos.t -> bool

val move_box : t -> src:Pos.t -> dst:Pos.t -> t

val move_robot : t -> pos:Pos.t -> t

val robot_pos : t -> Pos.t

val iter_boxes : t -> f:(Pos.t -> unit) -> unit

val of_list : string list -> t
