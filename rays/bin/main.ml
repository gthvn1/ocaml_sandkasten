open Rayslib

let () =
  (* Let's create a Green image  *)
  let p = Pixel.create ~r:0 ~g:200 ~b:0 in
  let image = Image.create ~width:200 ~height:100 ~pixel:p in
  print_string (Image.string_of_image image)
