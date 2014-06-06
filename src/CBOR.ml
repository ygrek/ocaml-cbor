(** CBOR encoder/decoder, RFC 7049 *)

module BE = EndianBytes.BigEndian_unsafe

exception Error of string

let (@@) f x = f x
let (>>) x f = f x
let fail fmt = Printf.ksprintf (fun s -> raise (Error s)) fmt

module Encode = struct

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

let need (s,i) n =
  if n > String.length s || !i + n > String.length s then
    fail "truncated: len %d pos %d need %d" (String.length s) !i n;
  let j = !i in
  i := !i + n;
  j

let get_byte (s,_ as r) = int_of_char @@ s.[need r 1]
let get_n (s,_ as r) n f = f s @@ need r n
let get_s (s,_ as r) n = String.sub s (need r n) n

let extract_number byte1 r =
  match byte1 land 0b11111 with
  | n when n < 24 -> n
  | 24 -> get_byte r
  | 25 -> get_n r 2 BE.get_int16
  | 26 -> Int32.to_int @@ get_n r 4 BE.get_int32
  | 27 -> Int64.to_int @@ get_n r 8 BE.get_int64
  | n -> fail "bad additional %d" n

let rec extract r =
  let byte1 = get_byte r in
  match byte1 lsr 5 with
  | 0 -> `Int (extract_number byte1 r)
  | 1 -> `Int (-1 - extract_number byte1 r)
  | 2 -> let n = extract_number byte1 r in `Bytes (get_s r n)
  | 3 -> let n = extract_number byte1 r in `Text (get_s r n)
  | 4 -> let n = extract_number byte1 r in `Array (Array.to_list @@ Array.init n (fun _ -> extract r))
  | 5 -> let n = extract_number byte1 r in `Map (Array.to_list @@ Array.init n (fun _ -> let a = extract r in let b = extract r in a,b))
  | n -> fail "FIXME major %d" n

let decode s =
  let i = ref 0 in
  let x = extract (s,i) in
  if !i <> String.length s then fail "extra data: len %d pos %d" (String.length s) !i;
  x

end (* Simple *)
