type color = Color.color
type point = { x : float; y : float }
type circle = { c : point; radius : float; stroke : color; fill : color }

type ellipse = {
  c : point;
  rx : float;
  ry : float;
  rotation : int;
  stroke : color;
  fill : color;
}

type polygon = { vertices : point list; stroke : color; fill : color }
type line = { a : point; b : point; stroke : color }

type shape =
  | Circle of circle
  | Ellipse of ellipse
  | Line of line
  | Polygon of polygon
  | Complex of shape list

type shapes = shape list

val point : int -> int -> point
val circle : ?c:point -> int -> shape
val rectangle : ?c:point -> int -> int -> shape
val ellipse : ?c:point -> int -> int -> shape
val complex : shape list -> shape
val line : ?a:point -> point -> shape
val polygon : point list -> shape
val with_stroke : color -> shape -> shape
val with_fill : color -> shape -> shape
