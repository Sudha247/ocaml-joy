val translate : float -> float -> Shape.shape -> Shape.shape
val scale : float -> Shape.shape -> Shape.shape
val deg_to_rad : float -> float
val rot : int -> Shape.point -> Shape.point
val rotate : int -> Shape.shape -> Shape.shape
val compose : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
