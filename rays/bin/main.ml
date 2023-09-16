open Rayslib

let () =
  (* Let's create a Green image  *)
  let image = Image.create ~width:256 ~height:256 in
  Image.transform image;
  print_string (Image.string_of_image image)
