let arithmetic_dsl () =
  let open Dsl_tuto.Arithmetic in
  let expr = Mul (Int 3, Int 4) in
  Printf.printf "expr = %d\n" (eval expr)

let build_dsl () =
  let open Dsl_tuto.Mini_build in
  build "hello"

let html_dsl () =
  let open Dsl_tuto.Html_dsl in
  Printf.printf "%s" (render welcome_page)

let () =
  arithmetic_dsl ();
  build_dsl ();
  html_dsl ()
