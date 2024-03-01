type color = Color.color
type 'a point = { x : 'a; y : 'a }

type circle = {
  c : float point;
  radius : float;
  stroke : color option;
  fill : color option;
}

type ellipse = {
  c : float point;
  rx : float;
  ry : float;
  stroke : color option;
  fill : color option;
}

type polygon = {
  vertices : float point list;
  stroke : color option;
  fill : color option;
}

type line = { a : float point; b : float point; stroke : color }

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
val line : ?a:float point -> float point -> shape
val polygon : float point list -> shape
val with_stroke : color -> shape -> shape
val with_fill : color -> shape -> shape
val no_stroke : shape -> shape
val no_fill : shape -> shape
