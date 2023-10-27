type shape
type shapes = shape list
type point = { x : int; y : int }

val render_shape : shape -> unit
val point : int -> int -> point
val circle : ?point:point -> int -> shape
val rectangle : ?point:point -> int -> int -> shape
val ellipse : ?point:point -> int -> int -> shape
val complex : shape list -> shape
val line : ?point_a:point -> point -> shape
val polygon : point list -> shape
val translate : int -> int -> shape -> shape
val show : shape list -> unit
val scale : float -> shape -> shape
val rotate : int -> shape -> shape
val compose : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
val repeat : int -> (shape -> shape) -> shape -> shape
val draw_axes : bool -> unit
val set_dimensions : int -> int -> unit
val init : unit -> unit
val close : unit -> unit
