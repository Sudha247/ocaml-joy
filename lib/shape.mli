type color = Color.color
type 'a point = { x : 'a; y : 'a }
type circle = { c : float point; radius : float; color : color }
type ellipse = { c : float point; rx : float; ry : float; color : color }
type polygon = { vertices : float point list; color : color }
type line = { a : float point; b : float point }

type shape =
  | Circle of circle
  | Ellipse of ellipse
  | Line of line
  | Polygon of polygon
  | Complex of shape list

type shapes = shape list

val point : int -> int -> float point
val ( /~ ) : float point -> float point -> float point
val ( -! ) : float point -> float -> float point
val ( /! ) : float point -> float -> float point
val ( *! ) : float point -> float -> float point
val pmap : ('a -> 'b) -> 'a point -> 'b point
val circle : ?c:float point -> int -> shape
val rectangle : ?c:float point -> int -> int -> shape
val ellipse : ?c:float point -> int -> int -> shape
val complex : shape list -> shape
<<<<<<< HEAD
val line : ?a:float point -> float point -> shape
val polygon : float point list -> shape
val with_color : color -> shape -> shape
=======
val line : ?a:point -> point -> shape
val polygon : point list -> shape
val with_color : Color.color -> shape -> shape
>>>>>>> 6df00cd (more merging ??)
