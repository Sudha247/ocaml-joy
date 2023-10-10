type shape
type shapes = shape list

type point

val render_shape : shape -> unit
val circle : ?x:int -> ?y:int -> int -> shape
val rectangle : ?x:int -> ?y:int -> int -> int -> shape
val ellipse : ?x:int -> ?y:int -> int -> int -> shape
val show : shape list -> unit

val rotate_pos : point -> float -> point 
val rotate : shape -> float -> shape

val draw_axes : bool -> unit
val set_dimensions : int -> int -> unit
val init : unit -> unit
val close : unit -> unit