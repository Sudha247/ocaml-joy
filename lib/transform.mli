type transformation = (Shape.shape -> Shape.shape)
val translate : float -> float -> transformation
val scale : float -> transformation
val rotate : int -> transformation
val compose : transformation -> transformation -> transformation
val repeat : int -> transformation -> transformation
