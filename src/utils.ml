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

