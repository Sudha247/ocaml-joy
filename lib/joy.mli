type 'a point = 'a Shape.point
type shape = Shape.shape
type shapes = Shape.shapes

type transformation = Transform.transformation

val point : int -> int -> float point
val circle : ?c:float point -> int -> shape
val rectangle : ?c:float point -> int -> int -> shape
val ellipse : ?c:float point -> int -> int -> shape
val line : ?a:float point -> float point -> shape
val polygon : float point list -> shape
val complex : shapes -> shape
<<<<<<< HEAD
val rotate : int -> transformation
val translate : int -> int -> transformation
val scale : float -> transformation
val compose : transformation -> transformation -> transformation
val repeat : int -> transformation -> transformation
=======
val rotate : int -> shape -> shape
val translate : float -> float -> shape -> shape
val scale : float -> shape -> shape
val compose : (shape -> shape) -> (shape -> shape) -> shape -> shape
val repeat : int -> (shape -> shape) -> shape -> shape
>>>>>>> fa1c1cf (Refactor Transform.compose to have more specific typing)
val context : Context.context option ref
val set_color : int * int * int -> unit
val background : int * int * int * int -> unit
val set_line_width : int -> unit
val init : ?line_width:int -> ?size:int * int -> ?axes:bool -> unit -> unit
val render : shape -> unit
val show : shapes -> unit
val write : ?filename:string -> unit -> unit
