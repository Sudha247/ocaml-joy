type context = {
  shapes: Shape.shape list ref;
  size: int * int;
  axes: bool;
}

val show : context -> Shape.shapes -> unit
val create : size:int * int -> axes:bool -> context
val write : context -> string
