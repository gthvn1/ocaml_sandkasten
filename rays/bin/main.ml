open Rayslib

let file = "/tmp/rays.ppm"

let () =
  let oc = open_out file in
  let image = Image.create ~width:256 ~height:256 in
  Image.transform image;
  Printf.fprintf oc "%s" (Image.string_of_image image)
