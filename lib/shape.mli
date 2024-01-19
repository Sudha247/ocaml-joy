type 'a point = { x : 'a; y : 'a }
type circle = { c : float point; radius : float }
type ellipse = { c : float point; rx : float; ry : float }
type polygon = float point list
type line = { a : float point; b : float point }

type shape =
  | Circle of circle
  | Ellipse of ellipse
  | Line of line
  | Polygon of polygon
  | Complex of shape list

type shapes = shape list

type color = Color.color

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
val line : ?a:float point -> float point -> shape
val polygon : float point list -> shape
val with_color : color -> shape -> shape
