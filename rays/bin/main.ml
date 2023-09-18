open Rayslib

let file = "/tmp/rays.ppm"

let () =
  let oc = open_out file in
  let image = Image.create ~columns:256 ~rows:256 in
  Image.transform image;
  Printf.fprintf oc "%s" (Image.string_of_ppm image)
