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
  let buf = Bytes.of_string "Ping" in
  Lwt_main.run
    ( create_socket socket_path >>= fun sk ->
      Lwt_unix.send sk buf 0 (Bytes.length buf) [] >>= fun bytes_send ->
      Lwt_fmt.eprintf "Send %d bytes\n%!" bytes_send )
