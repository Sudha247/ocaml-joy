val ( /~ ) : Shape.point -> Shape.point -> Shape.point
val ( -! ) : Shape.point -> float -> Shape.point
val ( /! ) : Shape.point -> float -> Shape.point
val ( *! ) : Shape.point -> float -> Shape.point
val pmap : (float -> float) -> Shape.point -> Shape.point
val tmap : ('a -> 'b) -> 'a * 'a -> 'b * 'b
val tmap3 : ('a -> 'b) -> 'a * 'a * 'a -> 'b * 'b * 'b
val tmap4 : ('a -> 'b) -> 'a * 'a * 'a * 'a -> 'b * 'b * 'b * 'b
val ( >> ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
val take : int -> 'a list -> 'a list * 'a list
val partition : int -> ?step:int -> 'a list -> 'a list list
val range : int -> int list
val to_radians : int -> float
