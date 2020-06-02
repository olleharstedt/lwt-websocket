open Unix
open Lwt
open Lwt_websocket
open Frame

type message =
  | Ping
  | Chat of string
[@@deriving json]

let to_json = [%to_json: message]
let of_json = [%of_json: message]

let _ =
  let v = Chat "Hey, what's up?" in
  let json = to_json v in
  print_endline json

  (*
  let buffer : Buffer.t = Buffer.create 255 in
  message_to_json buffer v;
  let content : string = Buffer.contents buffer in
  print_endline content
  *)

(**
 * Take care of a new client.
 *
 * @param channel
 * @return Lwt.t
 *)
let rec handle_client (channel : Channel.channel) : unit Lwt.t =
  let%lwt frame = channel#read_frame in 
  match frame with
    (** ping -> pong *)
    | PingFrame(msg) ->
      print_endline (Printf.sprintf "ping(%s)" msg);
      let%lwt _ = channel#write_pong_frame in
      handle_client channel (** wait for close frame from client *)

    (** text frame -> echo back *)
    | TextFrame text ->
      let%lwt _ = channel#write_text_frame text in
      handle_client channel (** wait for close frame from client *)

    | BinaryFrame ->
      print_endline "not supported";
      return ()

    | PongFrame(msg) ->
      print_endline "pong received";
      return ()

    (** close frame -> close frame *)
    | CloseFrame(status_code, body) ->
      print_endline "close frame received";

      (** http://tools.ietf.org/html/rfc6455#section-5.5.1

	  If an endpoint receives a Close frame and did not previously send a
	  Close frame, the endpoint MUST send a Close frame in response. 
      *)
      channel#write_close_frame

    | UndefinedFrame(msg) ->
      print_endline msg;
      return ()

(**
 * @param sock_listen
 * @return ?
 *)
let rec main (sock_listen : Lwt_unix.file_descr) : unit Lwt.t =
  let%lwt channel, addr = Channel.accept sock_listen in
  handle_client channel

(**
 * Main entry point.
 * Compile with
 *   ocamlfind opt -package extlib,num,bitstring,cryptokit,unix,lwt.unix,lwt_ppx,js_of_ocaml-ppx_deriving_json -thread -linkpkg lwt-websocket.cmxa example/server_blog.ml
 *)
let _ =
  let host : string = "127.0.0.1" in
  let port : int    = 8080 in

  let addr_inet : Unix.sockaddr = Unix.ADDR_INET (Unix.inet_addr_of_string host, port) in
  let sock_listen : Lwt_unix.file_descr = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in

  Lwt_unix.setsockopt sock_listen Unix.SO_REUSEADDR true;

  (** Setup listen socket *)
  Lwt_main.run (
    let%lwt () = Lwt_unix.bind sock_listen addr_inet in
    Lwt_unix.listen sock_listen 5;
    main sock_listen
  )
