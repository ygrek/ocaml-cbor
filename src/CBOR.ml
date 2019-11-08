(** CBOR encoder/decoder, RFC 7049 *)

module type Integer = sig
  type t
  val of_int : int -> t
  val of_int64 : int64 -> t
  val of_int32 : int32 -> t
  val to_int : t -> int
  val is_lt0 : t -> bool
  val minus_one_minus : t -> t
  val put : Buffer.t -> maj:int -> int -> unit
  val int : Buffer.t -> t -> unit
  val foo : t -> t (* XXX: check the function uses *)
  val bprintf_t : Buffer.t -> t -> unit
end

module type Simple = sig

type integer

type t =
[ `Null
| `Undefined
| `Simple of int
| `Bool of bool
| `Int of integer
| `Float of float
| `Bytes of string
| `Text of string
| `Array of t list
| `Map of (t * t) list
]

val encode : t -> string
val decode : string -> t

val to_diagnostic : t -> string

end

open Printf
module BE = EndianBytes.BigEndian_unsafe
module SE = EndianString.BigEndian_unsafe

exception Error of string

let (@@) f x = f x
let (|>) x f = f x
let list_iteri f l = let i = ref 0 in List.iter (fun x -> f !i x; incr i) l
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
  Buffer.add_string b (Bytes.unsafe_to_string s)

let hex_char x =
  assert (x >= 0 && x < 16);
  if x <= 9 then Char.chr @@ Char.code '0' + x
  else Char.chr @@ Char.code 'a' + x - 10

let to_hex s =
  let r = Bytes.create (String.length s * 2) in
  for i = 0 to String.length s - 1 do
    Bytes.set r (i*2) @@ hex_char @@ Char.code s.[i] lsr 4;
    Bytes.set r (i*2+1) @@ hex_char @@ Char.code s.[i] land 0b1111;
  done;
  Bytes.to_string r

end

module CInt = struct
  open Encode

  type t = int

  let two_min_int32 = 2 * Int32.(to_int min_int)
  let int64_max_int = Int64.of_int max_int

  let of_int x = x

  let to_int x = x

  let of_int64 x =
    if x > int64_max_int || x < 0L then fail "out-of-range: %Lu" x;
    Int64.to_int x

  let of_int32 x = Int32.to_int x

  let is_lt0 x = x <= 0

  let foo x =
    x - two_min_int32

  let minus_one_minus x =
    -1 - x

  let put b ~maj n =
    assert (n >= 0);
    if n < 24 then
      init b ~maj n
    else if n < 256 then
      begin init b ~maj 24; Buffer.add_char b @@ char_of_int n end
    else if n < 65536 then
      begin init b ~maj 25; put_n b 2 BE.set_int16 n end
    else if Int64.(compare (of_int n) 4294967296L) < 0 then
      begin init b ~maj 26; put_n b 4 BE.set_int32 @@ Int32.of_int n end
    else
      begin init b ~maj 27; put_n b 8 BE.set_int64 @@ Int64.of_int n end

  let int b n =
    let (maj,n) = if n < 0 then 1, -1 - n else 0, n in
    put b ~maj n

  let bprintf_t b x =
    bprintf b "%d" x
end

module CInt64 = struct
  open Encode

  type t = int64

  let two_min_int32 = Int64.(mul 2L @@ of_int32 Int32.min_int)

  let of_int = Int64.of_int

  let to_int = Int64.to_int (* TODO: check that value is in range for int *)

  let of_int64 x = x

  let of_int32 = Int64.of_int32

  let is_lt0 x = compare x 0L < 0

  let foo x =
    Int64.sub x two_min_int32

  let minus_one_minus x =
    Int64.(sub minus_one x)

  let put = CInt.put

  let put_int64 b ~maj n =
    assert (Int64.compare n 0L >= 0);
    if Int64.compare n 24L < 0 then
      init b ~maj @@ Int64.to_int n
    else if Int64.compare n 256L < 0 then
      begin init b ~maj 24; Buffer.add_char b @@ char_of_int @@ Int64.to_int n end
    else if Int64.compare n 65536L < 0 then
      begin init b ~maj 25; put_n b 2 BE.set_int16 @@ Int64.to_int n end
    else if Int64.compare n 4294967296L < 0 then
      begin init b ~maj 26; put_n b 4 BE.set_int32 @@ Int64.to_int32 n end
    else
      begin init b ~maj 27; put_n b 8 BE.set_int64 n end

  let int b n =
    let (maj,n) =
      if compare n 0L < 0 then
        1, Int64.(sub minus_one n)
      else
        0, n
    in
    put_int64 b ~maj n

  let bprintf_t b x =
    bprintf b "%Ld" x
end

module SimpleMake (Integer : Integer) = struct

type integer = Integer.t

type t =
[ `Null
| `Undefined
| `Simple of int
| `Bool of bool
| `Int of integer
| `Float of float
| `Bytes of string
| `Text of string
| `Array of t list
| `Map of (t * t) list
]

let encode item =
  let open Encode in
  let put = Integer.put in
  let b = start () in
  let rec write = function
  | `Null -> put b ~maj:7 22;
  | `Undefined -> put b ~maj:7 23;
  | `Bool false -> put b ~maj:7 20;
  | `Bool true -> put b ~maj:7 21;
  | `Simple n when (n >= 0 && n <= 23) || (n >= 32 && n <= 255) -> put b ~maj:7 n
  | `Simple n -> fail "encode: simple(%d)" n
  | `Int n -> Integer.int b n
  | `Float f -> init b ~maj:7 27; put_n b 8 BE.set_double f
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
let is_indefinite byte1 = get_additional byte1 = 31

let extract_number byte1 r =
  match get_additional byte1 with
  | n when n < 24 -> Integer.of_int n
  | 24 -> Integer.of_int @@ get_byte r
  | 25 -> Integer.of_int @@ get_n r 2 SE.get_uint16
  | 26 ->
    let n = Integer.of_int32 @@ get_n r 4 SE.get_int32 in
    if Integer.is_lt0 n then Integer.foo n else n
  | 27 ->
    (* XXX: why are negative elements of type 26 treated in a special way and
     * not those of type 27? *)
    Integer.of_int64 @@ get_n r 8 SE.get_int64
  | n -> fail "bad additional %d" n

let get_float16 s i =
  let half = Char.code s.[i] lsl 8 + Char.code s.[i+1] in
  let mant = half land 0x3ff in
  let value =
    match (half lsr 10) land 0x1f with (* exp *)
    | 31 when mant = 0 -> infinity
    | 31 -> nan
    | 0 -> ldexp (float mant) ~-24
    | exp -> ldexp (float @@ mant + 1024) (exp - 25)
  in
  if half land 0x8000 = 0 then value else ~-. value

exception Break

let extract_list byte1 r f =
  if is_indefinite byte1 then
    let l = ref [] in
    try while true do l := f r :: !l done; assert false with Break -> List.rev !l
  else
    let n = Integer.to_int @@ extract_number byte1 r in
    Array.to_list @@ Array.init n (fun _ -> f r)

let rec extract_pair r =
  let a = extract r in
  let b = try extract r with Break -> fail "extract_pair: unexpected break" in
  a,b
and extract_string byte1 r f =
  if is_indefinite byte1 then
    let b = Buffer.create 10 in
    try while true do Buffer.add_string b (f @@ extract r) done; assert false with Break -> Buffer.contents b
  else
    let n = extract_number byte1 r in get_s r @@ Integer.to_int n
and extract r =
  let byte1 = get_byte r in
  match byte1 lsr 5 with
  | 0 -> `Int (extract_number byte1 r)
  | 1 -> `Int (Integer.minus_one_minus @@ extract_number byte1 r)
  | 2 -> `Bytes (extract_string byte1 r (function `Bytes s -> s | _ -> fail "extract: not a bytes chunk"))
  | 3 -> `Text (extract_string byte1 r (function `Text s -> s | _ -> fail "extract: not a text chunk"))
  | 4 -> `Array (extract_list byte1 r extract)
  | 5 -> `Map (extract_list byte1 r extract_pair)
  | 6 -> let _tag = extract_number byte1 r in extract r
  | 7 ->
    begin match get_additional byte1 with
    | n when n < 20 -> `Simple n
    | 20 -> `Bool false
    | 21 -> `Bool true
    | 22 -> `Null
    | 23 -> `Undefined
    | 24 -> `Simple (get_byte r)
    | 25 -> `Float (get_n r 2 get_float16)
    | 26 -> `Float (get_n r 4 SE.get_float)
    | 27 -> `Float (get_n r 8 SE.get_double)
    | 31 -> raise Break
    | a -> fail "extract: (7,%d)" a
    end
  | _ -> assert false

let decode s : t =
  let i = ref 0 in
  let x = try extract (s,i) with Break -> fail "decode: unexpected break" in
  if !i <> String.length s then fail "decode: extra data: len %d pos %d" (String.length s) !i;
  x

let to_diagnostic item =
  let b = Buffer.create 10 in
  let put = Buffer.add_string b in
  let rec write = function
  | `Null -> put "null"
  | `Bool false -> put "false"
  | `Bool true -> put "true"
  | `Simple n -> bprintf b "simple(%d)" n
  | `Undefined -> put "undefined"
  | `Int n -> Integer.bprintf_t b n
  | `Float f ->
    begin match classify_float f with
    | FP_nan -> put "NaN"
    | FP_infinite -> put (if f < 0. then "-Infinity" else "Infinity")
    | FP_zero | FP_normal | FP_subnormal -> bprintf b "%g" f
    end
  | `Bytes s -> bprintf b "h'%s'" (Encode.to_hex s)
  | `Text s -> bprintf b "\"%s\"" s
  | `Array l ->
    put "[";
    l |> list_iteri (fun i x -> if i <> 0 then put ", "; write x);
    put "]"
  | `Map m ->
    put "{";
    m |> list_iteri (fun i (k,v) -> if i <> 0 then put ", "; write k; put ": "; write v);
    put "}"
  in
  write item;
  Buffer.contents b

end (* SimpleMake *)

module Simple = SimpleMake(CInt)
module Simple64 = SimpleMake(CInt64)
