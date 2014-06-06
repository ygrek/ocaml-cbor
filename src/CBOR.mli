
exception Error of string

module Simple : sig

type t =
[ `Int of int
| `Bytes of string
| `Text of string
| `Array of t list
| `Map of (t * t) list
]

val encode : t -> string
val decode : string -> t

end
