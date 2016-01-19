(***********************************************************************)
(*                                                                     *)
(*                   Bartholomew de La Villardière                     *)
(*                                                                     *)
(*                     http://www.bartholomew.fr/                      *)
(*                https://github.com/BartholomewPanda                  *)
(*                                                                     *)
(***********************************************************************)

open Lwt


let display_basic_stat host port =
    Mcquery.get_basic_stat host port
    >>= fun stat ->
        Mcquery.display_basic_stat stat;
        Lwt.return_unit

let _ =
    if Array.length Sys.argv <> 3 then
        Printf.eprintf "usage: %s host port\n" Sys.argv.(0)
    else
        let host = Sys.argv.(1) in
        let port = int_of_string Sys.argv.(2) in
        Lwt_main.run (display_basic_stat host port)

