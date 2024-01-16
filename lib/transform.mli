val translate : int -> int -> Shape.shape -> Shape.shape
val scale : float -> Shape.shape -> Shape.shape
val rotate : int -> Shape.shape -> Shape.shape
val compose : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
val repeat : int -> (Shape.shape -> Shape.shape) -> Shape.shape -> Shape.shape