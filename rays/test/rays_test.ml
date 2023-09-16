open Rayslib

let%expect_test _ =
  let pixel = Pixel.create ~r:10 ~g:5 ~b:210 in
  print_endline (Pixel.string_of_pixel pixel);
  [%expect {| 10 5 210 |}]

let%expect_test _ =
  let pixel = Pixel.create ~r:10 ~g:100 ~b:35 in
  let image = Image.create ~width:3 ~height:2 ~pixel:pixel in
  print_endline (Image.string_of_image image);
  [%expect {|
  P3
  3 2
  255
  10 100 35
  10 100 35
  10 100 35
  10 100 35
  10 100 35
  10 100 35
  |}]
