type t

val parse_line : string -> t

val move : t -> t

val draw_chart : t list -> string
