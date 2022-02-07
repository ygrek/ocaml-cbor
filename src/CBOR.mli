(** CBOR *)

exception Error of string

(** CBOR encoder/decoder, RFC 7049 *)
module Simple : sig

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
| `Tag of int * t
]

val encode : t -> string
val decode : string -> t
val decode_partial : string -> t * string

val to_diagnostic : t -> string

end

(**
  {{: https://fidoalliance.org/specs/fido-v2.0-ps-20190130/fido-client-to-authenticator-protocol-v2.0-ps-20190130.html#ctap2-canonical-cbor-encoding-form}CTAP2} decoder
*)
module CTAP2_canonical : sig
  type t = [
    | `Null
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

  exception Noncanonical of string

  val decode : string -> t
  val decode_partial : string -> t * string

  val to_simple : t -> Simple.t
end
