open Unix
open Lwt
open Lwt_websocket

(**
 * Take care of a new client.
 *
 * @param channel
 * @return Lwt.t
 *)
let rec handle_client (channel : Channel.channel) : unit Lwt.t =
  channel#read_frame >>= function
    (** ping -> pong *)
    | Frame.PingFrame(msg) ->
      print_endline (Printf.sprintf "ping(%s)" msg);
      channel#write_pong_frame >>= fun () ->
      handle_client channel (** wait for close frame from client *)

    (** text frame -> echo back *)
    | Frame.TextFrame text ->
      channel#write_text_frame text >>= fun () ->
      handle_client channel (** wait for close frame from client *)

    | Frame.BinaryFrame ->
      print_endline "not supported";
      return ()

    | Frame.PongFrame(msg) ->
      print_endline "pong received";
      return ()

    (** close frame -> close frame *)
    | Frame.CloseFrame(status_code, body) ->
      print_endline "close frame received";

      (** http://tools.ietf.org/html/rfc6455#section-5.5.1

	  If an endpoint receives a Close frame and did not previously send a
	  Close frame, the endpoint MUST send a Close frame in response. 
      *)
      channel#write_close_frame

    | Frame.UndefinedFrame(msg) ->
      print_endline msg;
      return ()

(**
 * @param sock_listen
 * @return ?
 *)
let rec main (sock_listen : Lwt_unix.file_descr) : unit Lwt.t =
  Channel.accept sock_listen >>= fun (channel, addr) ->
  handle_client channel

(**
 * Main entry point.
 * Compile with
 *   ocamlfind opt -package extlib,num,bitstring,cryptokit,unix,lwt.unix -thread -linkpkg lwt-websocket.cmxa example/server_blog.ml
 *)
let _ =
  let host : string = "127.0.0.1" in
  let port : int    = 8080 in

  let addr_inet : Unix.sockaddr = Unix.ADDR_INET (Unix.inet_addr_of_string host, port) in
  let sock_listen : Lwt_unix.file_descr = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in

  (** Setup listen socket *)
  Lwt_main.run (
    Lwt_unix.setsockopt sock_listen Unix.SO_REUSEADDR true;
    let%lwt () = Lwt_unix.bind sock_listen addr_inet in
    Lwt_unix.listen sock_listen 5;
    main sock_listen
  )
