open Rayslib

let%expect_test _ =
  let pixel = Pixel.create ~r:0.0 ~g:0.5 ~b:1.0 in
  print_endline (Pixel.string_of_pixel pixel);
  [%expect {| 0 127 255 |}]

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
