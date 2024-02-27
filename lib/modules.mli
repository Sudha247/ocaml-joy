module type Impl = sig
  type context

  val context : context option ref

  exception Context of string

  val fail : unit -> unit
  val init_context : int * int * int * int -> int -> int * int -> bool -> unit
  val axes : context -> bool
  val resolution : unit -> int * int
  val set_line_width : int -> unit
  val render_axes : unit -> unit
  val save : unit -> unit
  val restore : unit -> unit
  val write : context -> string -> unit
  val show : Shape.shapes -> unit
end

module type Backend = sig
  type context
  val context : context option ref

  type 'a point = 'a Shape.point
  type shape = Shape.shape
  type shapes = Shape.shapes
  type transformation = Transform.transformation
  type color = Color.color

  val point : int -> int -> float Shape.point
  val circle : ?c:float Shape.point -> int -> Shape.shape
  val rectangle : ?c:float Shape.point -> int -> int -> Shape.shape
  val polygon : float Shape.point list -> Shape.shape
  val ellipse : ?c:float Shape.point -> int -> int -> Shape.shape
  val line : ?a:float Shape.point -> float Shape.point -> Shape.shape
  val complex : Shape.shape list -> Shape.shape
  val rotate : int -> Transform.transformation
  val scale : float -> Transform.transformation
  val translate : int -> int -> Transform.transformation
  val with_stroke : color -> shape -> shape
  val with_fill : color -> shape -> shape
  val map_stroke : (color -> color) -> shape -> shape
  val map_fill : (color -> color) -> shape -> shape

  val white : color 
  val black : color 
  val red : color 
  val blue : color 
  val green : color 
  val yellow : color

  val compose :
    Transform.transformation ->
    Transform.transformation ->
    Transform.transformation

  val repeat : int -> Transform.transformation -> Transform.transformation
  val set_line_width : int -> unit
  val resolution : unit -> int * int

  val init :
    ?background:int * int * int * int ->
    ?line_width:int ->
    ?size:int * int ->
    ?axes:bool ->
    unit ->
    unit

  val write : ?filename:string -> unit -> unit
  val show : Shape.shapes -> unit
end

module Make : functor (B : Impl) -> Backend with type context = B.context
