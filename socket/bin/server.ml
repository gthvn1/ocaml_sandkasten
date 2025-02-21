(*
 * Domain: AF_UNIX for domain sockets
 * Type: SOCK_STREAM
 * File descriptor: 
 * Address: can be IP:PORT or path on the filesystem
 *
 * https://ocaml.github.io/odoc/lwt/lwt.unix/Lwt_unix/index.html#val-socket
 * https://www.geeksforgeeks.org/socket-in-computer-network/
 *
 * To test we can use:
 * ❯ echo "Hello from socat" | socat - UNIX-CONNECT:/tmp/socket_test
 *)

open Lwt.Infix

let socket_path = "/tmp/socket_test"

let create_socket (path : string) =
  (* Remove the old socket file if it exists. If the file doesn't exist
     unlink raises ENOENT so just ignore it. *)
  Lwt.catch
    (fun () -> Lwt_unix.unlink path)
    (function
      | Unix.Unix_error (Unix.ENOENT, _, _) -> Lwt.return_unit | e -> Lwt.fail e)
  >>= fun () ->
  Lwt_fmt.eprintf "Create a unix socket bind to %s\n%!" path >>= fun () ->
  let s = Lwt_unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Lwt_unix.bind s (Unix.ADDR_UNIX path) >>= fun () ->
  Lwt_unix.listen s 10;
  (* Allow up to 10 pending connections *)
  Lwt.return s

let rec accept_connections socket =
  let buffer = Bytes.create 64 in
  Lwt_unix.accept socket >>= fun (client_socket, _client_sockaddr) ->
  Lwt_fmt.eprintf "Client connected\n%!" >>= fun () ->
  Lwt_unix.read client_socket buffer 0 64 >>= fun bytes_read ->
  Lwt_fmt.eprintf "Received %d bytes\n%!" bytes_read >>= fun () ->
  Lwt_fmt.eprintf "%s\n%!" (Bytes.to_string buffer) >>= fun () ->
  Lwt.async (fun () -> Lwt_unix.close client_socket);
  accept_connections socket

let () =
  Lwt_main.run
    (create_socket socket_path >>= fun socket -> accept_connections socket)
