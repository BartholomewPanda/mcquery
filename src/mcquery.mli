(***********************************************************************)
(*                                                                     *)
(*                   Bartholomew de La Villardière                     *)
(*                                                                     *)
(*                     http://www.bartholomew.fr/                      *)
(*                https://github.com/BartholomewPanda                  *)
(*                                                                     *)
(***********************************************************************)

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

val get_basic_stat: string -> int -> basic_stat Lwt.t
val display_basic_stat: basic_stat -> unit

val get_full_stat: string -> int -> full_stat Lwt.t
val display_full_stat: full_stat -> unit

