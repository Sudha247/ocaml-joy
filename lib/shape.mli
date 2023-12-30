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

val point : float -> float -> point
val ( /~ ) : point -> point -> point
val ( -! ) : point -> float -> point
val ( /! ) : point -> float -> point
val circle : ?point:point -> float -> shape
val rectangle : ?point:point -> float -> float -> shape
val ellipse : ?point:point -> float -> float -> shape
val complex : shape list -> shape
val line : ?point:point -> point -> shape
val polygon : point list -> shape
