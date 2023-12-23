type point = { x : float; y : float }
type circle = { c : point; radius : float }
type ellipse = { c : point; rx : float; ry : float }
type polygon = point list
type line = { a : point; b : point }

type shape =
  | Circle of circle
  | Ellipse of ellipse
  | Line of line
  | Polygon of polygon
  | Complex of shape list

type shapes = shape list

type joy_context = {
  ctx : Cairo.context;
  surface : Cairo.Surface.t;
  size : point;
  filename : string;
}
val context : joy_context option ref
val fail : unit -> unit
val point : float -> float -> point
val circle : ?point:point -> float -> shape
val rectangle : ?point:point -> float -> float -> shape
val ellipse : ?point:point -> float -> float -> shape
val complex : shape list -> shape
val line : ?point:point -> point -> shape
val polygon : point list -> shape
val scale_point : point -> point -> float * float
val set_color : float * float * float * float -> unit
val background : float * float * float * float -> unit
val get_window_size : unit -> point
val bi_to_uni : point -> float * float
val denormalize : point -> point

val init :
  ?line_width:float -> ?size:float * float -> ?filename:string -> unit -> unit
val write : joy_context -> unit
