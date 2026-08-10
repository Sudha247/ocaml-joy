type point = Joy_core.Shape.point = { x : float; y : float }
type shape = Joy_core.Shape.shape
type transformation = shape -> shape
type color = Joy_core.Color.color

val point : int -> int -> point
val circle : ?c:point -> int -> shape
val rectangle : ?c:point -> int -> int -> shape
val ellipse : ?c:point -> int -> int -> shape
val line : ?a:point -> point -> shape
val polygon : point list -> shape
val complex : shape list -> shape

val rotate : int -> transformation
val translate : int -> int -> transformation
val scale : float -> transformation
val compose : transformation -> transformation -> transformation
val repeat : int -> transformation -> transformation
val with_stroke : color -> transformation
val with_fill : color -> transformation
val map_stroke : (color -> color) -> transformation
val map_fill : (color -> color) -> transformation

val black : color
val white : color
val red : color
val green : color
val blue : color
val yellow : color
val transparent : color
val rgb : int -> int -> int -> color
val color : ?a:float -> int -> int -> int -> color

module Noise = Joy_core.Noise

val init :
  ?background_color:color ->
  ?line_width:int ->
  ?size:int * int ->
  ?axes:bool ->
  unit ->
  unit

val show : shape list -> unit
val clear : unit -> unit
val render : unit -> unit
val set_line_width : int -> unit
val write : string -> unit
