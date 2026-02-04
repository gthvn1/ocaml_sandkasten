type mpoint = float * float
(** mpoint is a mathematical point *)

type spoint = int * int
(** spoint is a point on the screen *)

type view = { scale : float; width : float; height : float }

(* Some helpers *)

let win_width () = Graphics.size_x () - 1
let win_height () = Graphics.size_y () - 1

(** [to_screen (x, y)] converts mathematical coordinates [(x, y)] into screen
    coordinates. In screen coordinates, the origin [(0, 0)] is at the
    bottom-left corner of the screen, whereas in mathematical coordinates the
    origin is at the center of the screen. *)
let to_screen (x, y) ~view : spoint =
  let px = (x *. view.scale) +. (view.width /. 2.) in
  let py = (y *. view.scale) +. (view.height /. 2.) in
  (int_of_float px, int_of_float py)

let from_screen (x, y) ~view : mpoint =
  let x = float_of_int x and y = float_of_int y in
  let px = (x -. (view.width /. 2.)) /. view.scale in
  let py = (y -. (view.height /. 2.)) /. view.scale in
  (px, py)

let draw_axes () =
  let open Graphics in
  set_color black;
  (* Draw x axe *)
  moveto 0 (win_height () / 2);
  lineto (win_width ()) (win_height () / 2);
  (* Draw y axe *)
  moveto (win_width () / 2) 0;
  lineto (win_width () / 2) (win_height ())

let draw_fun f ~first ~last ~color ~view ~step =
  let open Graphics in
  set_color color;

  let rec loop x =
    if x > last then ()
    else
      let px, py = to_screen (x, f x) ~view in
      lineto px py;
      loop (x +. step)
  in
  (* we go to the first position *)
  let px, py = to_screen (first, f first) ~view in
  moveto px py;
  loop first

type state = {
    last_pressed : char
  ; mouse_x : int
  ; mouse_y : int
  ; scale : float
}

let () =
  let open Graphics in
  open_graph "";
  auto_synchronize false;

  let rec loop s =
    clear_graph ();

    (* current view *)
    let current_width = win_width () in
    let current_height = win_height () in
    let view =
      {
        scale = s.scale
      ; width = current_width |> float_of_int
      ; height = current_height |> float_of_int
      }
    in

    draw_axes ();

    (* draw some function *)
    let first, last = from_screen (0, current_width) ~view in
    draw_fun sin ~first ~last ~color:green ~view ~step:0.1;

    (* print the last key pressed if any *)
    set_color black;
    moveto 10 10;
    draw_string (Printf.sprintf "Last key pressed %c" s.last_pressed);

    (* display the scale *)
    moveto 10 30;
    draw_string (Printf.sprintf "Scale %.2f" s.scale);

    (* display area info *)
    set_color blue;
    moveto 10 50;
    let x_min, y_min = from_screen ~view (0, 0) in
    let x_max, y_max = from_screen ~view (current_width, current_height) in
    draw_string
      (Printf.sprintf "(%.2f,%.2f) -> (%.2f, %.2f)" x_min y_min x_max y_max);

    (* Draw a circle to the corresponding to the mouse_x position and the function applied to it *)
    let px, _ = from_screen (s.mouse_x, s.mouse_y) ~view in
    let py = sin px in
    moveto 10 70;
    draw_string (Printf.sprintf "Red dot position (%.2f,%.2f)" px py);

    let sx, sy = to_screen (px, py) ~view in
    set_color red;
    fill_circle sx sy 3;

    (* synchronize and wait for next event *)
    synchronize ();

    let status = wait_next_event [ Key_pressed; Mouse_motion ] in
    if status.keypressed then
      match status.key with
      | 'q' -> ()
      | '+' -> loop { s with last_pressed = '+'; scale = s.scale +. 1.0 }
      | '-' ->
          loop { s with last_pressed = '-'; scale = max (s.scale -. 1.0) 1. }
      | c ->
          loop
            {
              s with
              last_pressed = c
            ; mouse_x = status.mouse_x
            ; mouse_y = status.mouse_y
            }
    else loop { s with mouse_x = status.mouse_x; mouse_y = status.mouse_y }
  in
  loop { last_pressed = ' '; mouse_x = 0; mouse_y = 0; scale = 50.0 };
  close_graph ()
