(***********************************************************************)
(*                                                                     *)
(*                   Bartholomew de La Villardière                     *)
(*                                                                     *)
(*                     http://www.bartholomew.fr/                      *)
(*                https://github.com/BartholomewPanda                  *)
(*                                                                     *)
(***********************************************************************)

open Lwt

type packet_type =
    | Handshake
    | Stat

type basic_stat =
    {motd:       Bytes.t;
     gamemode:   Bytes.t;
     map:        Bytes.t;
     numplayers: int;
     maxplayers: int;
     hostport:   int;
     hostip:     Bytes.t}

type full_stat =
    {motd:       Bytes.t;
     gamemode:   Bytes.t;
     gameid:     Bytes.t;
     version:    Bytes.t;
     plugins:    Bytes.t list;
     map:        Bytes.t;
     numplayers: int;
     maxplayers: int;
     hostport:   int;
     hostip:     Bytes.t}

type stat =
    | BasicStat of basic_stat
    | FullStat of full_stat

let packet_type_to_char = function
    | Handshake -> '\x09'
    | Stat      -> '\x00'

(*
 * Make a simple packet:
 * *--------------------------------------------------*
 * | Magic      | byte, byte | \xFE\xFD               |
 * | Type       | byte       | Handshake or Stat      |
 * | Session ID | int32      |                        |
 * | Payload    |            | Empty, challenge, etc. |
 * *--------------------------------------------------*
 *)
let make_packet type_ session payload =
    let buffer = Utils.Buffer.create (7 + (String.length payload)) in
    Utils.Buffer.add_char buffer '\xFE';
    Utils.Buffer.add_char buffer '\xFD';
    Utils.Buffer.add_char buffer (packet_type_to_char type_);
    Utils.Buffer.add_int  buffer session;
    String.iter (fun c -> Buffer.add_char buffer c) payload;
    Utils.Buffer.to_bytes buffer

(*
 * Check if the packet header is valid by checking its Type and its Session ID.
 *)
let check_packet response length type_ session =
    length >= 4 &&
    Bytes.get response 0 = (packet_type_to_char type_) &&
    Bytes.get response 1 = (char_of_int ((session land 0xFF000000) lsr 24)) &&
    Bytes.get response 2 = (char_of_int ((session land 0x00FF0000) lsr 16)) &&
    Bytes.get response 3 = (char_of_int ((session land 0x0000FF00) lsr 8)) &&
    Bytes.get response 4 = (char_of_int (session land 0x000000FF))

(*
 * Send a handshake request to the server and get the challenge from the server
 * response.
 *)
let handshake socket sockaddr =
    let session = 0x00000001 in
    let packet = make_packet Handshake session "" in
    Network.send_and_recv socket sockaddr packet
    >>= fun (response, length) ->
        if not (check_packet response length Handshake session) then
            Lwt.fail_with "invalid handshake response"
        else begin
            (* Extract the challenge field from a handshake response. *)
            let stream = Utils.Stream.of_bytes response in
            Utils.Stream.junk_n stream 5;
            let challenge = int_of_string (Utils.Stream.get_str stream) in
            let buffer = Utils.Buffer.create 4 in
            Utils.Buffer.add_int buffer challenge;
            Lwt.return (Buffer.to_bytes buffer)
        end

(*
 * Send a basic stat request and retrieve all the informations.
 *)
let basic_stat_request socket sockaddr challenge =
    let session = 0x00000001 in
    let packet = make_packet Stat session challenge in
    Network.send_and_recv socket sockaddr packet
    >>= fun (response, length) ->
        if not (check_packet response length Stat session) then
            Lwt.fail_with "invalid basic stat response"
        else
            let stream     = Utils.Stream.of_bytes response in
            (* skip Type and Session ID *)
            Utils.Stream.junk_n stream 5;
            let motd       = Utils.Stream.get_str stream in
            let gamemode   = Utils.Stream.get_str stream in
            let map        = Utils.Stream.get_str stream in
            let numplayers = int_of_string (Utils.Stream.get_str stream) in
            let maxplayers = int_of_string (Utils.Stream.get_str stream) in
            let hostport   = Utils.Stream.get_short stream in
            let hostip     = Utils.Stream.get_str stream in
            Lwt.return {motd; gamemode; map; numplayers; maxplayers; hostport; hostip}

(*
 * Send a full stat request and retrieve all the informations.
 * TODO: get other information.
 *)
let full_stat_request socket sockaddr challenge =
    let session = 0x00000001 in
    let payload = Bytes.extend challenge 0 4 in
    Bytes.fill payload 4 4 '\xFF';
    let packet = make_packet Stat session payload in
    Network.send_and_recv socket sockaddr packet
    >>= fun (response, length) ->
        if not (check_packet response length Stat session) then
            Lwt.fail_with "invalid full stat response"
        else
            let stream     = Utils.Stream.of_bytes response in
            (* skip Type and Session ID *)
            Utils.Stream.junk_n stream 5;
            let motd       = Utils.Stream.get_str stream in
            let gamemode   = Utils.Stream.get_str stream in
            let map        = Utils.Stream.get_str stream in
            let numplayers = int_of_string (Utils.Stream.get_str stream) in
            let maxplayers = int_of_string (Utils.Stream.get_str stream) in
            let hostport   = int_of_string (Utils.Stream.next_n stream 2) in
            let hostip     = Utils.Stream.get_str stream in
            Lwt.return {motd; gamemode; map; numplayers; maxplayers; hostport; hostip; gameid="plop";version="1";plugins=[]}

let get_basic_stat host port =
    let socket, sockaddr = Network.make_socket host port in
    handshake socket sockaddr
    >>= fun challenge ->
        basic_stat_request socket sockaddr challenge

let get_full_stat host port =
    let socket, sockaddr = Network.make_socket host port in
    handshake socket sockaddr
    >>= fun challenge ->
        full_stat_request socket sockaddr challenge

let display_basic_stat (stat: basic_stat) =
    Printf.printf "MOTD: %s\n" stat.motd;
    Printf.printf "Gamemode: %s\n" stat.gamemode;
    Printf.printf "Map: %s\n" stat.map;
    Printf.printf "Players: %u/%u\n" stat.numplayers stat.maxplayers;
    Printf.printf "Server: %s:%u\n" stat.hostip stat.hostport

