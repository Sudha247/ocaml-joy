module type Backend = sig
  type context

  val context : context option ref

  exception Context of string

  val fail : unit -> unit
  val init_context : float -> int * int -> bool -> unit

  (* Getters *)
  val axes : context -> bool
  val resolution : unit -> int * int

  (* Setters *)
  val set_color : int * int * int -> unit
  val background : int * int * int * int -> unit
  val set_line_width : int -> unit
  val render_axes : unit -> unit
  val save : unit -> unit
  val restore : unit -> unit
  val write : context -> string -> unit
  val show : Shape.shapes -> unit
end

module Joy (B : Backend) = struct
  let context = B.context

  type 'a point = 'a Shape.point
  type shape = Shape.shape
  type shapes = Shape.shapes
  type transformation = Transform.transformation

  let point = Shape.point
  let circle = Shape.circle
  let rectangle = Shape.rectangle
  let polygon = Shape.polygon
  let ellipse = Shape.ellipse
  let line = Shape.line
  let complex = Shape.complex
  let rotate = Transform.rotate
  let scale = Transform.scale
  let translate = Transform.translate
  let compose = Transform.compose
  let repeat = Transform.repeat
  let set_color = B.set_color
  let background = B.background
  let set_line_width = B.set_line_width
  let resolution = B.resolution

  let init ?(line_width = 2) ?(size = (800, 800)) ?(axes = false) () =
    B.init_context (float_of_int line_width /. 1000.) size axes

  let write ?(filename = "joy.png") () =
    match !B.context with
    | Some ctx ->
        if B.axes ctx then B.render_axes ();
        B.write ctx filename
    | None -> B.fail ()

  let show shapes = B.show shapes
end
