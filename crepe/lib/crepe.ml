let serve () =
  (* 1. Create a socket *)
  let sock_domain = Unix.PF_INET in
  let sock_type = Unix.SOCK_STREAM in
  let sock_fd = Unix.socket sock_domain sock_type 0 in
  print_endline "Socket created" ;
  (* We want to be able to reuse local addr for binding *)
  Unix.setsockopt sock_fd Unix.SO_REUSEADDR true ;

  (* 2. Bind socket to an address and port *)
  let port = 8080 in
  let addr = "127.0.0.1" in
  let sock_addr = Unix.ADDR_INET (Unix.inet_addr_of_string addr, port) in
  Unix.bind sock_fd sock_addr ;
  Printf.printf "Socket binded to %s:%d\n" addr port ;

  (* 3. Listen for incoming connections *)
  let max_pending_reqs = 1 in
  Unix.listen sock_fd max_pending_reqs ;
  print_endline "Listenning..." ;

  (* 4. Accept connections *)
  let client_fd, client_addr = Unix.accept sock_fd in
  let () =
    match client_addr with
    | ADDR_UNIX s ->
        Printf.printf "Connection accepted from %s\n" s
    | ADDR_INET (a, p) ->
        Printf.printf "Connection accepted from %s:%d\n"
          (Unix.string_of_inet_addr a)
          p
  in

  (* 5. Read data *)
  let buffer = Bytes.init 1024 (fun _ -> 'a') in
  let bytes_recv = Unix.recv client_fd buffer 0 1024 [] in
  Printf.printf "Received %d bytes\n" bytes_recv ;
  Printf.printf "--START--\n%s\n--END--\n" (Bytes.sub_string buffer 0 bytes_recv) ;

  (* 6. Send data *)
  let msg = Bytes.of_string "HTTP/1.1 200 OK" in
  let bytes_send = Unix.send client_fd msg 0 (Bytes.length msg) [] in
  Printf.printf "Send %d bytes to client\n" bytes_send ;

  (* 7. Close all connections *)
  Unix.close client_fd ;
  Unix.close sock_fd ;
  print_endline "crepe server closed all connections."
