(** CBOR encoder/decoder, RFC 7049 *)

let (@@) f x = f x
let (>>) x f = f x

module Encode = struct

module BE = EndianBytes.BigEndian_unsafe

let start () = Buffer.create 10

let init b ~maj add =
  assert (maj >= 0 && maj < 8);
  assert (add >= 0 && add < 32);
  Buffer.add_char b @@ char_of_int @@ (maj lsl 5) lor add

let put b ~maj n =
  assert (n >= 0);
  if n < 24 then
    init b ~maj n
  else if n < 256 then
    begin init b ~maj 24; Buffer.add_char b @@ char_of_int n end
  else if n < 65536 then
    begin init b ~maj 25; let s = Bytes.create 2 in BE.set_int16 s 0 n; Buffer.add_string b s end
  else if n < 4294967296 then (* optcomp int32 *)
    begin init b ~maj 26; let s = Bytes.create 4 in BE.set_int32 s 0 @@ Int32.of_int n; Buffer.add_string b s end
  else
    (* FIXME max int64 check *)
    begin init b ~maj 27; let s = Bytes.create 8 in BE.set_int64 s 0 @@ Int64.of_int n; Buffer.add_string b s end

let int b n =
  let (maj,n) = if n < 0 then 1, -1 - n else 0, n in
  put b ~maj n

end

module Simple = struct

type t =
[ `Int of int
| `Bytes of string
| `Text of string
| `Array of t list
| `Map of (t * t) list
]

let encode item =
  let open Encode in
  let b = start () in
  let rec write = function
  | `Int n -> int b n
  | `Bytes s -> put b ~maj:2 (String.length s); Buffer.add_string b s
  | `Text s -> put b ~maj:3 (String.length s); Buffer.add_string b s
  | `Array l -> put b ~maj:4 (List.length l); List.iter write l
  | `Map m -> put b ~maj:5 (List.length m); List.iter (fun (a,b) -> write a; write b) m
  in
  write item;
  Buffer.contents b

let decode _s = `Array []

end (* Simple *)
