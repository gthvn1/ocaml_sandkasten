(** [to_screen (x, y)] converts mathematical coordinates [(x, y)] into screen
    coordinates. In screen coordinates, the origin [(0, 0)] is at the
    bottom-left corner of the screen, whereas in mathematical coordinates the
    origin is at the center of the screen. *)
let to_screen (x, y) ~scale : int * int =
  let width = float_of_int @@ Graphics.size_x () in
  let height = float_of_int @@ Graphics.size_y () in
  let px = (x *. scale) +. (width /. 2.) in
  let py = (y *. scale) +. (height /. 2.) in
  (int_of_float px, int_of_float py)

let from_screen (x, y) ~scale : float * float =
  let x = float_of_int x and y = float_of_int y in
  let width = float_of_int @@ Graphics.size_x () in
  let height = float_of_int @@ Graphics.size_y () in
  let px = (x -. (width /. 2.)) /. scale in
  let py = (y -. (height /. 2.)) /. scale in
  (px, py)

let draw_axes () =
  let open Graphics in
  set_color black;
  (* Draw x axe *)
  moveto 0 (size_y () / 2);
  lineto (size_x ()) (size_y () / 2);
  (* Draw y axe *)
  moveto (size_x () / 2) 0;
  lineto (size_x () / 2) (size_y ())

let draw_fun f ~first ~last ~color ~scale =
  let open Graphics in
  set_color color;
  let step = 0.1 in

  let rec loop x =
    if x > last then ()
    else
      let y = f x in
      let px, py = to_screen (x, y) ~scale in
      lineto px py;
      loop (x +. step)
  in
  (* we go to the first position *)
  let px, py = to_screen (first, f first) ~scale in
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

  let rec loop s =
    clear_graph ();

    draw_axes ();

    (* draw some function *)
    let first, last = from_screen (0, size_x ()) ~scale:s.scale in
    draw_fun sin ~first ~last ~color:green ~scale:s.scale;

    (*
    draw_fun cos ~first:(-10.0) ~last:10.0 ~color:blue;
    draw_fun
      (fun x -> (2. *. x *. x) -. (3. *. x) +. 1.)
      ~first:(-10.0) ~last:10.0 ~color:red;
    *)

    (* print the last key pressed if any *)
    set_color black;
    moveto 10 10;
    draw_string (Printf.sprintf "Last key pressed %c" s.last_pressed);

    (* display the mouse position *)
    let px, py = from_screen (s.mouse_x, s.mouse_y) ~scale:s.scale in
    moveto 10 30;
    draw_string (Printf.sprintf "Mouse position (%.2f,%.2f)" px py);

    (* display the scale *)
    moveto 10 50;
    draw_string (Printf.sprintf "Scale %.2f" s.scale);

    (* synchronize and wait for next event *)
    synchronize ();

    let status = wait_next_event [ Key_pressed; Mouse_motion ] in
    if status.keypressed then
      match status.key with
      | 'q' -> ()
      | '+' -> loop { s with last_pressed = '+'; scale = s.scale +. 1.0 }
      | '-' ->
          loop { s with last_pressed = '-'; scale = max (s.scale -. 1.0) 0. }
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
