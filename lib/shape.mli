type point
type shape
type shapes = shape list

val render_shape : shape -> unit
val point : int -> int -> point
val circle : ?point:point -> int -> shape
val rectangle : ?point:point -> int -> int -> shape
val ellipse : ?point:point -> int -> int -> shape
val complex : shape list -> shape
val line : ?point_a:point -> point -> shape
val translate : int -> int -> shape -> shape
val show : shape list -> unit
val scale : float -> shape -> shape
val rotate : int -> shape -> shape
val compose : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
val draw_axes : bool -> unit
val set_dimensions : int -> int -> unit
val init : unit -> unit
val close : unit -> unit
