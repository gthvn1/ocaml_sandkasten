open Rayslib

let%expect_test _ =
  let image = Image.create ~rows:2 ~columns:3 in
  print_endline (Image.string_of_ppm image);
  [%expect
    {|
  P3
  3 2
  255
  255 255 255  255 255 255  255 255 255
  255 255 255  255 255 255  255 255 255
  |}]
