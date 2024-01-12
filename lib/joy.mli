type point = Shape.point
type shape = Shape.shape
type shapes = Shape.shapes

val point : float -> float -> point
val circle : ?c:point -> float -> shape
val rectangle : ?c:point -> float -> float -> shape
val ellipse : ?c:point -> float -> float -> shape
val line : ?a:point -> point -> shape
val polygon : point list -> shape
val complex : shapes -> shape
val rotate : int -> shape -> shape
val translate : float -> float -> shape -> shape
val scale : float -> shape -> shape
val compose : (shape -> shape) -> (shape -> shape) -> shape -> shape
val repeat : int -> (shape -> shape) -> shape -> shape
val context : Context.context option ref
val set_color : float * float * float -> unit
val background : float * float * float * float -> unit
val set_line_width : float -> unit

val init :
  ?line_width:float -> ?size:float * float -> ?axes:bool -> unit -> unit

val render : shape -> unit
val show : shapes -> unit
val write : ?filename:string -> unit -> unit
