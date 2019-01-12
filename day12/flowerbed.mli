type t

type conversions

val parse_conversion_lines : string list -> conversions

val parse_initial_config : string -> t

val get_num_flowers : t -> int

val max : t -> int

val min : t -> int

val get_next_flowerbed : conversions -> t -> t

val sum : t -> int

include Core.Comparator.S with type t := t
