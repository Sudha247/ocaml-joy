type shape
type shapes = shape list

val render_shape : shape -> unit
val circle : ?x:int -> ?y:int -> int -> shape
val rectangle : ?x:int -> ?y:int -> int -> int -> shape
val ellipse : ?x:int -> ?y:int -> int -> int -> shape
val line : ?x1:int -> ?y1:int -> int -> int -> shape
val translate : int -> int -> shape -> shape
val show : shape list -> unit
val rotate : int -> shape -> shape
val draw_axes : bool -> unit
val set_dimensions : int -> int -> unit
val init : unit -> unit
val close : unit -> unit
