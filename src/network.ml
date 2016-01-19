(***********************************************************************)
(*                                                                     *)
(*                   Bartholomew de La Villardière                     *)
(*                                                                     *)
(*                     http://www.bartholomew.fr/                      *)
(*                https://github.com/BartholomewPanda                  *)
(*                                                                     *)
(***********************************************************************)

open Lwt


let get_inet_addr host =
    try
        Unix.inet_addr_of_string host
    with Failure("inet_addr_of_string") ->
        let host = Unix.gethostbyname host in
        host.Unix.h_addr_list.(0)

let make_socket host port =
    let sockaddr = (Lwt_unix.ADDR_INET (get_inet_addr host, port)) in
    let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_DGRAM (Unix.getprotobyname "udp").Unix.p_proto in
    (socket, sockaddr)

let send socket sockaddr data =
    Lwt_unix.sendto socket data 0 (String.length data) [] sockaddr

let recv ?(size=50) socket =
    let buffer = Bytes.create size in
    Lwt_unix.recvfrom socket buffer 0 size []
    >>= fun (length, _) ->
        Lwt.return (buffer, length)

let send_and_recv socket sockaddr ?(size=50) data =
    send socket sockaddr data
    >>= fun _ ->
        recv ~size socket
    >>= Lwt.return

