(** CBOR encoder/decoder, RFC 7049 *)

exception Error of string

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

module Simple : sig include Simple with type integer = int end
module Simple64 : sig include Simple with type integer = int64 end
