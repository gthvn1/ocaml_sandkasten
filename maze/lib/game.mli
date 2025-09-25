val of_file : string -> Board.t * State.t

val move_left : board:Board.t -> state:State.t -> State.t

val move_down : board:Board.t -> state:State.t -> State.t

val move_up : board:Board.t -> state:State.t -> State.t

val move_right : board:Board.t -> state:State.t -> State.t
