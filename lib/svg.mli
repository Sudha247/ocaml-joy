module S : Modules.Backend
module Backend :
  sig
    val context : S.context option ref
    type 'a point = 'a Shape.point
    type shape = Shape.shape
    type shapes = Shape.shapes
    type transformation = Transform.transformation
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
    val compose :
      Transform.transformation ->
      Transform.transformation -> Transform.transformation
    val repeat : int -> Transform.transformation -> Transform.transformation
    val set_color : int * int * int -> unit
    val background : int * int * int * int -> unit
    val set_line_width : int -> unit
    val resolution : unit -> int * int
    val init :
      ?line_width:int -> ?size:int * int -> ?axes:bool -> unit -> unit
    val write : ?filename:string -> unit -> unit
    val show : Shape.shapes -> unit
  end
