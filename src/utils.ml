(***********************************************************************)
(*                                                                     *)
(*                   Bartholomew de La Villardière                     *)
(*                                                                     *)
(*                     http://www.bartholomew.fr/                      *)
(*                https://github.com/BartholomewPanda                  *)
(*                                                                     *)
(***********************************************************************)

module Stream =
struct
    include Stream

    let junk_n stream n =
        for i = 0 to n - 1 do
            Stream.junk stream
        done

    let next_n stream n =
        let buffer = Bytes.create n in
        for i = 0 to n - 1 do
            Bytes.set buffer i (Stream.next stream)
        done;
        buffer

    let get_str stream =
        let buffer = Buffer.create 20 in
        while Stream.peek stream <> Some '\x00' do
            Buffer.add_char buffer (Stream.next stream)
        done;
        Stream.junk stream;
        Buffer.to_bytes buffer

    let get_short stream =
        let b1 = int_of_char (next stream) in
        let b2 = int_of_char (next stream) in
        (b2 lsl 8) + b1
end

let bytes_to_short b =
    let b1 = int_of_char (Bytes.get b 0) in
    let b2 = int_of_char (Bytes.get b 1) in
    (b2 lsl 8) + b1

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

