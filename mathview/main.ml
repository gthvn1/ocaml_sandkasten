type mpoint = float * float
(** Mathematical point [(x, y)] in Cartesian coordinates. *)

type spoint = int * int
(** Pixel coordinates [(x, y)] in screen space. *)

type view = { scale : float; width : float; height : float }
(** Current viewport settings: [scale] in pixels per math unit and
    [width]/[height] in pixels. *)

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

(** [from_screen (x, y)] converts screen coordinates [(x, y)] with origin at the
    bottom-left into mathematical coordinates whose origin is the center of the
    screen. *)
let from_screen (x, y) ~view : mpoint =
  let x = float_of_int x and y = float_of_int y in
  let px = (x -. (view.width /. 2.)) /. view.scale in
  let py = (y -. (view.height /. 2.)) /. view.scale in
  (px, py)

let draw_axes () =
  let open Graphics in
  set_color black;
  (* Draw x-axis *)
  moveto 0 (win_height () / 2);
  lineto (win_width ()) (win_height () / 2);
  (* Draw y-axis *)
  moveto (win_width () / 2) 0;
  lineto (win_width () / 2) (win_height ())

(** [draw_fun f ~first ~last ~color ~view ~step] draws the graph of [f] on the
    interval [[first, last]] using step size [step] in mathematical units. *)
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
  ; scale : float
  ; mouse_pos : spoint
  ; x0 : int option
}
(** UI state carried across iterations of the event loop. *)

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
    moveto 10 50;
    let x_min, y_min = from_screen ~view (0, 0) in
    let x_max, y_max = from_screen ~view (current_width, current_height) in
    draw_string
      (Printf.sprintf "(%.2f,%.2f) -> (%.2f, %.2f)" x_min y_min x_max y_max);

    (* display x0 if set and add it on the function graph *)
    let () =
      match s.x0 with
      | Some sx ->
          let mx0, _ = from_screen (sx, 0) ~view in
          let my0 = sin mx0 in

          let sx0, sy0 = to_screen (mx0, my0) ~view in
          set_color blue;
          fill_circle sx0 sy0 3;

          (* compute the slope *)
          let mx1, _ = from_screen s.mouse_pos ~view in
          let my1 = sin mx1 in

          (* avoid division by zero *)
          if abs_float (mx1 -. mx0) > 1e-6 then (
            let a = (my1 -. my0) /. (mx1 -. mx0) in
            let b = my0 -. (a *. mx0) in

            (* display equation *)
            moveto 10 70;
            draw_string (Printf.sprintf "f(x) = %.3f x + %.3f" a b);

            (* draw the line *)
            let x_min, _ = from_screen (0, 0) ~view in
            let x_max, _ = from_screen (current_width, 0) ~view in

            let y_min = (a *. x_min) +. b in
            let y_max = (a *. x_max) +. b in

            let sx1, sy1 = to_screen (x_min, y_min) ~view in
            let sx2, sy2 = to_screen (x_max, y_max) ~view in

            set_color blue;
            moveto sx1 sy1;
            lineto sx2 sy2);

          set_color black;
          moveto 10 90;
          draw_string (Printf.sprintf "x0 (%.2f,%.2f)" mx0 my0)
      | None ->
          moveto 10 90;
          draw_string (Printf.sprintf "x0")
    in

    (* Draw a circle at the mouse x-position on the function graph *)
    let mx, _ = from_screen s.mouse_pos ~view in
    let my = sin mx in
    moveto 10 110;
    draw_string (Printf.sprintf "Red dot position (%.2f,%.2f)" mx my);

    let sx, sy = to_screen (mx, my) ~view in
    set_color red;
    fill_circle sx sy 3;

    (* synchronize and wait for next event *)
    synchronize ();

    let status = wait_next_event [ Key_pressed; Mouse_motion; Button_down ] in
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
            ; mouse_pos = (status.mouse_x, status.mouse_y)
            }
    else if status.button then
      match s.x0 with
      | Some _ -> loop { s with x0 = None }
      | None -> loop { s with x0 = Some status.mouse_x }
    else loop { s with mouse_pos = (status.mouse_x, status.mouse_y) }
  in
  loop { last_pressed = ' '; scale = 50.0; mouse_pos = (0, 0); x0 = None };
  close_graph ()
