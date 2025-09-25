let of_file (filename : string) : Board.t * State.t =
  (* we will go through the list twice but it allows a clean separation of concerns.
     Board cares only about walls/floors.
     State cares only about player/boxes.
     The cost is negligible for maze files (a few KB) *)
  let ic = open_in filename in
  let rec loop acc =
    try
      let s = input_line ic in
      loop (s :: acc)
    with End_of_file -> close_in ic ; List.rev acc
  in
  let lst = loop [] in
  let board = Board.of_list lst in
  let state = State.of_list lst in
  (board, state)

let rec try_push_box (board : Board.t) (state : State.t) ~(src : Pos.t)
    ~(dir : Pos.t) : State.t option =
  let x, y = src in
  let dx, dy = dir in
  let dst_x, dst_y = (x + dx, y + dy) in
  match Board.is_floor board ~x:dst_x ~y:dst_y with
  | false ->
      Printf.printf "Box is blocked by a wall\n" ;
      flush stdout ;
      None
  | true ->
      (* Check if there is a box *)
      if State.is_box_at ~pos:(dst_x, dst_y) state then
        (* There is a box, try to push the next one if any *)
        match try_push_box board state ~src:(dst_x, dst_y) ~dir with
        | None ->
            None
        | Some new_state ->
            Some (State.move_box ~src ~dst:(dst_x, dst_y) new_state)
      else
        (* there is no box so just move this one *)
        Some (State.move_box ~src ~dst:(dst_x, dst_y) state)

let move_robot (board : Board.t) (state : State.t) ~(dir : Pos.t) : State.t =
  let rx, ry = State.robot_pos state in
  let dx, dy = dir in
  let dst_x, dst_y = (rx + dx, ry + dy) in
  (* Checking if we reach a floor *)
  match Board.is_floor board ~x:dst_x ~y:dst_y with
  | false ->
      Printf.printf "Robot blocked by a wall\n" ;
      flush stdout ;
      state
  | true ->
      if State.is_box_at state ~pos:(dst_x, dst_y) then
        match try_push_box board state ~src:(dst_x, dst_y) ~dir with
        | None ->
            state
        | Some new_state ->
            State.move_robot ~pos:(dst_x, dst_y) new_state
      else State.move_robot ~pos:(dst_x, dst_y) state

let move_left ~(board : Board.t) ~(state : State.t) : State.t =
  move_robot board state ~dir:(-1, 0)

let move_down ~(board : Board.t) ~(state : State.t) : State.t =
  move_robot board state ~dir:(0, 1)

let move_up ~(board : Board.t) ~(state : State.t) : State.t =
  move_robot board state ~dir:(0, -1)

let move_right ~(board : Board.t) ~(state : State.t) : State.t =
  move_robot board state ~dir:(1, 0)
