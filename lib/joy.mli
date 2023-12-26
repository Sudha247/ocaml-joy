type point = Shape.point
type shape = Shape.shape
type shapes = Shape.shapes

val point : float -> float -> point
val circle : ?point:point -> float -> shape
val rectangle : ?point:point -> float -> float -> shape
val ellipse : ?point:point -> float -> float -> shape
val line : ?point:point -> point -> shape
val polygon : point list -> shape
val complex : shapes -> shape
val rotate : int -> shape -> shape
val translate : float -> float -> shape -> shape
val scale : float -> shape -> shape
val compose : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
val context : Context.context option ref
val set_color : float * float * float -> unit
val background : float * float * float * float -> unit
val set_line_width : float -> unit

val init :
  ?line_width:float ->
  ?size:float * float ->
  ?filename:string ->
  ?axes:bool ->
  unit ->
  unit

val render : shape -> unit
val show : shapes -> unit
