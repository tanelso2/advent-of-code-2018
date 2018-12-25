type t

val empty : unit -> t

val add : t -> int -> t

val remove : t -> (t * int)
