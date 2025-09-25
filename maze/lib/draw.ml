open Graphics
module B = Board
module S = State

(* Initialize graphics window *)
let init_window (board : B.t) : unit =
  let height = B.(height board * cell_size) in
  let width = B.(width board * cell_size) in
  open_graph (Printf.sprintf " %dx%d" width height) ;
  auto_synchronize false ;
  Printf.printf "Window size (width: %d, height: %d)\n" width height ;
  Printf.printf "Press q to quit...\n%!"

(* Draw the robot *)
let draw_robot (x, y) =
  set_color red ;
  fill_circle
    ((x * B.cell_size) + (B.cell_size / 2))
    ((y * B.cell_size) + (B.cell_size / 2))
    (B.cell_size / 2)

(* Draw all boxes *)
let draw_boxes (state : State.t) =
  set_color blue ;
  S.iter_boxes
    ~f:(fun (x, y) ->
      fill_circle
        ((x * B.cell_size) + (B.cell_size / 2))
        ((y * B.cell_size) + (B.cell_size / 2))
        (B.cell_size / 2) )
    state

(* Draw a single cell *)
let draw_cell (x, y) ~(ty : B.cell) : unit =
  let color = match ty with Wall -> black | Floor -> white in
  set_color color ;
  fill_rect x y B.cell_size B.cell_size

let draw_board (board : B.t) : unit =
  B.iteri_cells
    (fun x y cell -> draw_cell ~ty:cell (x * B.cell_size, y * B.cell_size))
    board

(* Render the full state *)
let render board state =
  clear_graph () ;
  draw_board board ;
  draw_robot (S.robot_pos state) ;
  draw_boxes state ;
  synchronize ()

(* Read key press *)
let read_key () = Graphics.read_key ()

(* Close graphics *)
let close_window () = Graphics.close_graph ()
