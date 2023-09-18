open Format
open Rayslib

let file = "/tmp/rays.ppm"

let raytraced_image =
  (*
   * let's use a 16:9 aspect ratio image
   * It allows to see errors when manipulating x and y axis. Otherwise with 1:1
   * ratio we can mixed value and it seems ok.
   *)
  let aspect_ratio = 16. /. 9. in
  let image_width = 800 in
  let image_height = float_of_int image_width /. aspect_ratio |> int_of_float in
  (* setup camera *)
  let focal_length = 1.0 in
  let viewport_height = 2.0 in
  let viewport_width = viewport_height *. aspect_ratio in
  let camera_center = Vec3.orig in
  "aspect_ratio:    " ^ string_of_float aspect_ratio |> print_endline;
  "image_width:     " ^ string_of_int image_width |> print_endline;
  "image_height:    " ^ string_of_int image_height |> print_endline;
  "viewport_width:  " ^ string_of_float viewport_width |> print_endline;
  "viewport_height: " ^ string_of_float viewport_height |> print_endline;
  "focal length:    " ^ string_of_float focal_length |> print_endline;
  "camera_center:   " ^ Vec3.string_of_vec3 camera_center |> print_endline;
  Image.create ~columns:image_width ~rows:image_height

let () =
  let oc = open_out file in
  let fmt = formatter_of_out_channel oc in
  let image = raytraced_image in
  (* until ray tracing is implemented we can use the transform function *)
  Image.transform image;
  fprintf fmt "%s" (Image.string_of_ppm image);
  pp_print_flush fmt ();
  close_out oc
