open Format
open Rayslib

let default_fname = "/tmp/rays.ppm"

let () =
  let fname = if (Array.length Sys.argv) < 2 then default_fname else Sys.argv.(1) in
  let oc = open_out fname in
  let fmt = formatter_of_out_channel oc in
  let image = Ray.raytrace () in
  fprintf fmt "%s" (Image.string_of_ppm image);
  printf "Image dumped into %s\n" fname;
  pp_print_flush fmt ();
  close_out oc
