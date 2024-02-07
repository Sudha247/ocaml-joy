let context = Context.context

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
let set_color = Context.set_color
let background = Context.background
let set_line_width = Context.set_line_width

let init ?(line_width = 2) ?(size = (800, 800)) ?(axes = false) () =
  Context.init_context (float_of_int line_width) size axes

let write ?(filename = "joy.png") () =
  match !Context.context with
  | Some ctx ->
      if ctx.axes then Render.render_axes ();
      Context.write ctx filename
  | None -> Context.fail ()

let render shape = Render.render shape
let show shapes = Render.show shapes
