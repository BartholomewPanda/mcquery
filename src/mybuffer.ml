(***********************************************************************)
(*                                                                     *)
(*                   Bartholomew de La Villardière                     *)
(*                                                                     *)
(*                     http://www.bartholomew.fr/                      *)
(*                https://github.com/BartholomewPanda                  *)
(*                                                                     *)
(***********************************************************************)

include Buffer

let add_int buffer nb =
    let mask = ref 0xFF000000 in
    let shift = ref 24 in
    for i = 0 to 3 do
        Buffer.add_char buffer (char_of_int ((nb land !mask) lsr !shift));
        mask := !mask lsr 8;
        shift := !shift - 8
    done
