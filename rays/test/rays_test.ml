open Rays

let%expect_test _ =
  Image.hello ();
  [%expect {| Hello from Image |}]
