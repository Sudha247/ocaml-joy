type shape
type shapes = shape list
type color = RGB of int * int * int | Transparent

val render_shape : shape -> unit
val circle : ?x:int -> ?y:int -> ?fill:color -> ?stroke:color -> int -> shape
val rectangle : ?x:int -> ?y:int -> ?fill:color -> ?stroke:color -> int -> int -> shape
val ellipse : ?x:int -> ?y:int -> ?fill:color -> ?stroke:color -> int -> int -> shape
val line : ?x1:int -> ?y1:int -> int -> int -> shape
val translate : int -> int -> shape -> shape
val show : shape list -> unit
val scale : float -> shape -> shape
val rotate : int -> shape -> shape

val compose : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c

val draw_axes : bool -> unit
val set_dimensions : int -> int -> unit
val init : unit -> unit
val close : unit -> unit