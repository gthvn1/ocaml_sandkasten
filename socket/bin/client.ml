(*
 * From client we need to
 *   - create socket
 *   - connect to the socket
 *   - send data
 *   - receive data
 *   - close the socket
 *
 * https://ocaml.github.io/odoc/lwt/lwt.unix/Lwt_unix/index.html
 *)
open Lwt.Infix

let socket_path = "/tmp/socket_test"

let create_socket (path : string) =
  let sk = Lwt_unix.(socket PF_UNIX SOCK_STREAM 0) in
  Lwt_unix.(connect sk (ADDR_UNIX path)) >>= fun () -> Lwt.return sk

let () =
  Lwt_main.run
    ( Lwt_fmt.printf "Enter a message: %!" >>= fun () ->
      Lwt_io.read_line Lwt_io.stdin >>= fun user_input ->
      create_socket socket_path >>= fun sk ->
      let buf = Bytes.of_string user_input in
      Lwt_unix.send sk buf 0 (Bytes.length buf) [] >>= fun bytes_send ->
      Lwt_fmt.eprintf "Send %d bytes\n%!" bytes_send >>= fun () ->
      let buf = Bytes.create 64 in
      Lwt_unix.read sk buf 0 64 >>= fun bytes_read ->
      Lwt_fmt.eprintf "Received %d bytes\n%!" bytes_read >>= fun () ->
      let received = Bytes.to_string buf in
      Lwt_fmt.eprintf "%s\n%!" received )
