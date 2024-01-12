val translate : float -> float -> Shape.shape -> Shape.shape
val scale : float -> Shape.shape -> Shape.shape
val rotate : int -> Shape.shape -> Shape.shape
val compose : (Shape.shape -> Shape.shape) -> (Shape.shape -> Shape.shape) -> Shape.shape -> Shape.shape
val repeat : int -> (Shape.shape -> Shape.shape) -> Shape.shape -> Shape.shape