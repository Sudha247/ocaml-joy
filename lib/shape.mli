type shape
type shapes = shape list

val render_shape : shape -> unit
val circle : ?x:int -> ?y:int -> int -> shape
val rectangle : ?x:int -> ?y:int -> int -> int -> shape
val ellipse : ?x:int -> ?y:int -> int -> int -> shape
val triangle : ?x1:int -> ?y1:int -> ?x2:int -> ?y2:int -> ?x3:int -> ?y3:int -> unit -> shape
val show : shape list -> unit

val init : unit -> unit
val close : unit -> unit