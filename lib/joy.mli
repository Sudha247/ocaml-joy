type 'a point = 'a Shape.point
type shape = Shape.shape
type shapes = Shape.shapes
type transformation = Transform.transformation
type color = Color.color

val point : int -> int -> float point
val circle : ?c:float point -> int -> shape
val rectangle : ?c:float point -> int -> int -> shape
val ellipse : ?c:float point -> int -> int -> shape
val line : ?a:float point -> float point -> shape
val polygon : float point list -> shape
val complex : shapes -> shape
val with_stroke : color -> shape -> shape
val with_fill : color -> shape -> shape
val rotate : int -> transformation
val translate : int -> int -> transformation
val scale : float -> transformation
val compose : transformation -> transformation -> transformation
val repeat : int -> transformation -> transformation
val map_stroke : (color -> color) -> shape -> shape
val map_fill : (color -> color) -> shape -> shape
val context : Context.context option ref
val set_line_width : int -> unit
val black : color
val white : color
val red : color
val green : color
val blue : color
val yellow : color
val transparent : int * int * int * int
val opaque : color -> int * int * int * int

val init :
  ?background:color ->
  ?line_width:int ->
  ?size:int * int ->
  ?axes:bool ->
  unit ->
  unit

val render : shape -> unit
val show : shapes -> unit
val write : ?filename:string -> unit -> unit
