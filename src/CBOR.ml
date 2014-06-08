(** CBOR encoder/decoder, RFC 7049 *)

open Printf
module BE = EndianBytes.BigEndian_unsafe

exception Error of string

let (@@) f x = f x
let (|>) x f = f x
let fail fmt = ksprintf (fun s -> raise (Error s)) fmt

module Encode = struct

let start () = Buffer.create 10

let init b ~maj add =
  assert (maj >= 0 && maj < 8);
  assert (add >= 0 && add < 32);
  Buffer.add_char b @@ char_of_int @@ (maj lsl 5) lor add

let put_n b n f x =
  let s = Bytes.create n in
  f s 0 x;
  Buffer.add_string b s

let put b ~maj n =
  assert (n >= 0);
  if n < 24 then
    init b ~maj n
  else if n < 256 then
    begin init b ~maj 24; Buffer.add_char b @@ char_of_int n end
  else if n < 65536 then
    begin init b ~maj 25; put_n b 2 BE.set_int16 n end
  else if n < 4294967296 then (* optcomp int32 *)
    begin init b ~maj 26; put_n b 4 BE.set_int32 @@ Int32.of_int n end
  else
    (* FIXME max int64 check *)
    begin init b ~maj 27; put_n b 8 BE.set_int64 @@ Int64.of_int n end

let int b n =
  let (maj,n) = if n < 0 then 1, -1 - n else 0, n in
  put b ~maj n

let hex_char x =
  assert (x >= 0 && x < 16);
  if x <= 9 then Char.chr @@ Char.code '0' + x
  else Char.chr @@ Char.code 'a' + x - 10

let to_hex s =
  let r = Bytes.create (String.length s * 2) in
  for i = 0 to String.length s - 1 do
    r.[i*2] <- hex_char @@ Char.code s.[i] lsr 4;
    r.[i*2+1] <- hex_char @@ Char.code s.[i] land 0b1111;
  done;
  Bytes.to_string r

end

module Simple = struct

type t =
[ `Null
| `Undefined
| `Simple of int
| `Bool of bool
| `Int of int
| `Float of float
| `Bytes of string
| `Text of string
| `Array of t list
| `Map of (t * t) list
]

let encode item =
  let open Encode in
  let b = start () in
  let rec write = function
  | `Null -> put b ~maj:7 22;
  | `Undefined -> put b ~maj:7 23;
  | `Bool false -> put b ~maj:7 20;
  | `Bool true -> put b ~maj:7 21;
  | `Simple n when (n >= 0 && n <= 23) || (n >= 32 && n <= 255) -> put b ~maj:7 n
  | `Simple n -> fail "encode: simple(%d)" n
  | `Int n -> int b n
  | `Float f -> put b ~maj:7 27; put_n b 8 BE.set_double f
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

let get_additional byte1 = byte1 land 0b11111

let extract_number byte1 r =
  match get_additional byte1 with
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
  | 7 ->
    begin match get_additional byte1 with
    | n when n < 20 -> `Simple n
    | 20 -> `Bool false
    | 21 -> `Bool true
    | 22 -> `Null
    | 23 -> `Undefined
    | 24 -> `Simple (get_byte r)
    | 25 -> fail "FIXME float16"
    | 26 -> `Float (get_n r 4 BE.get_float)
    | 27 -> `Float (get_n r 8 BE.get_double)
    | a -> fail "FIXME (7,%d)" a
    end
  | n -> fail "FIXME major %d" n

let decode s =
  let i = ref 0 in
  let x = extract (s,i) in
  if !i <> String.length s then fail "extra data: len %d pos %d" (String.length s) !i;
  x

let to_diagnostic item =
  let b = Buffer.create 10 in
  let rec write = function
  | `Null -> bprintf b "null"
  | `Bool false -> bprintf b "false"
  | `Bool true -> bprintf b "true"
  | `Simple n -> bprintf b "simple(%d)" n
  | `Undefined -> bprintf b "undefined"
  | `Int n -> bprintf b "%d" n
  | `Float f ->
    begin match classify_float f with
    | FP_nan -> bprintf b "NaN"
    | FP_infinite -> bprintf b (if f < 0. then "-Infinity" else "Infinity")
    | FP_zero | FP_normal | FP_subnormal -> bprintf b "%g" f
    end
  | `Bytes s -> bprintf b "h'%s'" (Encode.to_hex s)
  | `Text s -> bprintf b "'%s'" s
  | `Array l ->
    bprintf b "[";
    l |> List.iteri (fun i x -> if i <> 0 then bprintf b ", "; write x);
    bprintf b "]"
  | `Map m ->
    bprintf b "{";
    m |> List.iteri (fun i (k,v) -> if i <> 0 then bprintf b ", "; write k; bprintf b ": "; write v);
    bprintf b "}"
  in
  write item;
  Buffer.contents b

end (* Simple *)
