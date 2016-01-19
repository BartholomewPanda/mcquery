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


module Buffer =
struct
    include Buffer

    let add_int buffer nb =
        let mask = ref 0xFF000000 in
        let shift = ref 24 in
        for i = 0 to 3 do
            Buffer.add_char buffer (char_of_int ((nb land !mask) lsr !shift));
            mask := !mask lsr 8;
            shift := !shift - 8
        done
end

