type shape
type shapes = shape list

val render_shape : shape -> unit
val circle : ?x:int -> ?y:int -> int -> shape
val rectangle : ?x:int -> ?y:int -> int -> int -> shape
val ellipse : ?x:int -> ?y:int -> int -> int -> shape
val triangle: int -> int -> int -> int -> int -> int -> shape 
val show : shape list -> unit

val draw_axes : bool -> unit
val init : unit -> unit
val close : unit -> unit