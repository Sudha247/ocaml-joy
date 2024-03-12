module Context = Context
module Backend_cairo = Backend_cairo
module Backend_svg = Backend_svg
module Backend_lazy = Backend_lazy

type context = Context.context
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
val rotate : int -> transformation
val translate : int -> int -> transformation
val scale : float -> transformation
val compose : transformation -> transformation -> transformation
val repeat : int -> transformation -> transformation
val random : ?min:int -> int -> int
val frandom : ?min:float -> float -> float
val noise : float list -> float
val fractal_noise : ?octaves:int -> float list -> float
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
val init : ?size:int * int -> ?line_width:int -> ?axes:bool -> unit -> unit
val init_svg : ?size:int * int -> ?axes:bool -> unit -> unit
val write : ?filename:string -> unit -> unit
val show : ?ctx:context -> shapes -> unit
val set_line_width : ?ctx:context -> int -> unit
