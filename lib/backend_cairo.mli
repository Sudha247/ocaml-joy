type context = {
  cairo_ctx : Cairo.context;
  surface : Cairo.Surface.t;
  size : int * int;
  background_color : Color.color;
  axes : bool;
}

val set_color : context -> Color.color -> unit
val set_background : context -> Color.color -> unit
val set_line_width : context -> int -> unit
val show : context -> Shape.shapes -> unit
val write : context -> string -> unit

val create :
  background_color:Color.color ->
  line_width:int ->
  size:int * int ->
  axes:bool ->
  context
