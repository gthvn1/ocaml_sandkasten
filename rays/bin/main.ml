open Format
open Rayslib

let file = "/tmp/rays.ppm"

let () =
  let oc = open_out file in
  let fmt = formatter_of_out_channel oc in
  let image = Ray.raytrace () in
  fprintf fmt "%s" (Image.string_of_ppm image);
  pp_print_flush fmt ();
  close_out oc
