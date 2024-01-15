type transform = (Shape.shape -> Shape.shape)
val translate : float -> float -> transform
val scale : float -> transform
val rotate : int -> transform
val compose : transform -> transform -> transform
val repeat : int -> transform -> transform