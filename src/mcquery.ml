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
     players:    Bytes.t list;
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
    let buffer = Mybuffer.create (7 + (String.length payload)) in
    Mybuffer.add_string buffer "\xFE\xFD";
    Mybuffer.add_char buffer (packet_type_to_char type_);
    Mybuffer.add_int  buffer session;
    String.iter (fun c -> Buffer.add_char buffer c) payload;
    Mybuffer.to_bytes buffer

(*
 * Check if the packet header is valid by checking its Type and its Session ID.
 *)
let check_packet response length type_ session =
    let packet_session = Utils.bytes_to_int (Bytes.sub response 1 4) in
    length >= 4 && response.[0] = (packet_type_to_char type_) &&
    packet_session = session

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
            let packet_content = Bytes.sub response 5 (length - 5) in
            let stream         = Stream.from (Utils.split_string packet_content) in
            let challenge      = int_of_string (Stream.next stream) in
            let buffer         = Mybuffer.create 4 in
            Mybuffer.add_int buffer challenge;
            Lwt.return (Mybuffer.to_bytes buffer)
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
        else begin
            (* skip Type and Session ID *)
            let packet_content = Bytes.sub response 5 (length - 5) in
            let stream         = Stream.from (Utils.split_string packet_content) in
            let motd           = Stream.next stream in
            let gamemode       = Stream.next stream in
            let map            = Stream.next stream in
            let numplayers     = int_of_string (Stream.next stream) in
            let maxplayers     = int_of_string (Stream.next stream) in
            (* the port field is not null terminated *)
            let address        = Stream.next stream in
            (* split address into (port, ip) *)
            let hostport       = Utils.bytes_to_short (Bytes.sub address 0 2) in
            let hostip         = Bytes.sub address 2 (Bytes.length address - 2) in
            Lwt.return {motd; gamemode; map; numplayers; maxplayers; hostport; hostip}
        end

(*
 * In a full stat response, information are a list of key, value.
 *)
let get_keys_values stream =
    let values = Hashtbl.create 10 in
    let loop   = ref true in
    while !loop do
        try
            let key = Stream.next stream in
            if Bytes.length key = 0 then
                loop := false
            else
                let value = Stream.next stream in
                Hashtbl.add values key value
        with
            Stream.Failure -> loop := false
    done;
    values

let get_players stream =
    let loop    = ref true in
    let players = ref [] in
    while !loop do
        try
            let player = Stream.next stream in
            if Bytes.length player = 0 then
                loop := false
            else
                players := player :: !players
        with
            Stream.Failure -> loop := false
    done;
    !players

(*
 * Send a full stat request and retrieve all the informations.
 * TODO: get other information.
 *)
let full_stat_request socket sockaddr challenge =
    let session = 0x00000001 in
    let payload = Bytes.extend challenge 0 4 in
    Bytes.fill payload 4 4 '\xFF';
    let packet = make_packet Stat session payload in
    Network.send_and_recv socket sockaddr ~size:1024 packet
    >>= fun (response, length) ->
        if not (check_packet response length Stat session) then
            Lwt.fail_with "invalid full stat response"
        else begin
            (* skip Type, Session ID and Padding *)
            let packet_content = Bytes.sub response 16 (length - 16) in
            let stream         = Stream.from (Utils.split_string packet_content) in
            let kv             = get_keys_values stream in
            (* skip meaningless padding *)
            ignore (Stream.next stream);
            ignore (Stream.next stream);
            let players        = get_players stream in
            Lwt.return
                {motd       = Hashtbl.find kv "hostname";
                 gamemode   = Hashtbl.find kv "gametype";
                 gameid     = Hashtbl.find kv "game_id";
                 version    = Hashtbl.find kv "version";
                 plugins    = [Hashtbl.find kv "plugins"];
                 map        = Hashtbl.find kv "map";
                 numplayers = int_of_string (Hashtbl.find kv "numplayers");
                 maxplayers = int_of_string (Hashtbl.find kv "maxplayers");
                 hostip     = Hashtbl.find kv "hostip";
                 hostport   = int_of_string (Hashtbl.find kv "hostport");
                 players    = players}
        end

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

let display_full_stat (stat: full_stat) =
    Printf.printf "MOTD: %s\n" stat.motd;
    Printf.printf "Gamemode: %s\n" stat.gamemode;
    Printf.printf "Map: %s\n" stat.map;
    Printf.printf "Players: %u/%u:\n" stat.numplayers stat.maxplayers;
    List.iter (Printf.printf " - %s\n") stat.players;
    Printf.printf "Server: %s:%u\n" stat.hostip stat.hostport;
    Printf.printf "Gameid: %s\n" stat.gameid;
    Printf.printf "Version: %s\n" stat.version;
    Printf.printf "Plugins:\n";
    List.iter (Printf.printf " - %s\n") stat.plugins

