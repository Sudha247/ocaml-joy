type shape
type shapes = shape list

type point = {x: int; y: int}

val render_shape : shape -> unit
val circle : ?x:int -> ?y:int -> int -> shape
val rectangle : ?x:int -> ?y:int -> int -> int -> shape
val ellipse : ?x:int -> ?y:int -> int -> int -> shape
val line : ?x1:int -> ?y1:int -> int -> int -> shape
val polygon : point list -> shape
val translate : int -> int -> shape -> shape
val show : shape list -> unit
val scale : float -> shape -> shape
val rotate : int -> shape -> shape

val compose : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c

val draw_axes : bool -> unit
val set_dimensions : int -> int -> unit
val init : unit -> unit
val close : unit -> unit
