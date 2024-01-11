let () =
  Dream.run @@ Dream.logger
  @@ Dream.router
       [
         Dream.get "/" (fun _ -> Dream.html "Good morning");
         Dream.get "/echo/:word" (fun request ->
             Dream.html (Dream.param request "word"));
       ]
