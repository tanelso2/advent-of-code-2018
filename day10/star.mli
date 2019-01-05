type t

val create : (int * int) -> (int * int) -> t

val move : t -> t

val draw_chart : t list -> string
