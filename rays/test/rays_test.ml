open Rayslib

let%expect_test _ =
  let pixel = Pixel.create ~r:10 ~g:5 ~b:210 in
  print_endline (Pixel.string_of_pixel pixel);
  [%expect {| 10 5 210 |}]

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
