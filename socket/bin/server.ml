(*
 * Domain: AF_UNIX for domain sockets
 * Type: SOCK_STREAM
 * File descriptor: 
 * Address: can be IP:PORT or path on the filesystem
 *
 * To test we can use:
 * ❯ echo "Hello from socat" | socat - UNIX-CONNECT:/tmp/socket_test
 *)

let create_server (path : string) =
  let open Unix in
  (* Remove the socket file if it exists *)
  (try unlink path with Unix_error (ENOENT, _, _) -> ());

  Printf.eprintf "Create sock addr using %s\n%!" path;
  let sock_addr = ADDR_UNIX path in
  Printf.eprintf "Create the socket for UNIX STREAM\n%!";
  let sock_fd = socket PF_UNIX SOCK_STREAM 0 in
  setsockopt sock_fd SO_REUSEADDR true;
  Printf.eprintf "Bind sock addr with created socket\n%!";
  bind sock_fd sock_addr;
  listen sock_fd 10;

  (* Ensure cleanup on exit *)
  at_exit (fun () -> close sock_fd);
  at_exit (fun () -> unlink path);

  sock_fd

let serve_request (client_socket : Unix.file_descr) =
  let buffer = Bytes.create 64 in
  let received_bytes = Unix.recv client_socket buffer 0 64 [] in
  Printf.eprintf "received %d bytes\n%!" received_bytes;
  Printf.eprintf "%s\n%!" (Bytes.to_string buffer);
  Unix.close client_socket

let () =
  let socket_path = "/tmp/socket_test" in
  (*
   * Currently we are running the server manually so we can intercept ctrl-c to terminate 
   * properly. We don't need to clean up the socket file as it will be cleaned up on exit
   * using at_exit. Also we handle the sigint when accepting a client connection.
   *)
  Sys.set_signal Sys.sigint
    (Sys.Signal_handle (fun _ -> print_endline "Terminating..."; exit 0));

  let server_socket = create_server socket_path in
  while true do
    try
      Printf.eprintf "Waiting for client....\n%!";
      let client_socket, client_addr = Unix.accept server_socket in
      let () =
        match client_addr with
        | Unix.ADDR_UNIX s -> Printf.eprintf "We are connected to %s !!!\n%!" s
        | _ -> failwith "Not expected..."
      in
      serve_request client_socket
    with Unix.Unix_error (EINTR, _, _) -> ()
  done
