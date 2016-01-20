(***********************************************************************)
(*                                                                     *)
(*                   Bartholomew de La Villardière                     *)
(*                                                                     *)
(*                     http://www.bartholomew.fr/                      *)
(*                https://github.com/BartholomewPanda                  *)
(*                                                                     *)
(***********************************************************************)

let bytes_to_short b =
    let b1 = int_of_char (Bytes.get b 0) in
    let b2 = int_of_char (Bytes.get b 1) in
    (b2 lsl 8) + b1

let bytes_to_int b =
    (int_of_char b.[3]) +
    ((int_of_char b.[2]) lsr 8) +
    ((int_of_char b.[1]) lsr 16) +
    ((int_of_char b.[0]) lsr 24)

(*
 * Function used to build a stream that return strings
 *)
let split_string str =
    let pos = ref 0 in
    let length = Bytes.length str in
    let split _ =
        if !pos >= length then
            None
        else begin
            let index = Bytes.index_from str !pos '\x00' in
            let result = Some (Bytes.sub str !pos (index - !pos)) in
            pos := index + 1;
            result
        end
    in
    split

