open Rayslib

let%expect_test _ =
  let pixel = Pixel.create ~r:0.0 ~g:0.5 ~b:1.0 in
  print_endline (Pixel.string_of_pixel pixel);
  [%expect {| 0 127 255 |}]
