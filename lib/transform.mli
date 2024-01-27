type transformation = Shape.shape -> Shape.shape

val translate : int -> int -> transformation
val scale : float -> transformation
val rotate : int -> transformation
val compose : transformation -> transformation -> transformation
val repeat : int -> transformation -> transformation
val map_colors : (Color.color -> Color.color) -> transformation
