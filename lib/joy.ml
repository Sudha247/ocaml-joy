let context = Context.context

type point = Shape.point
type shape = Shape.shape
type shapes = Shape.shapes

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
let set_color = Context.set_color
let background = Context.background
let set_line_width = Context.set_line_width

let init ?(line_width = 0.002) ?(size = (800., 800.)) ?(filename = "cairo.png")
    ?(axes = false) () =
  Context.init_context line_width size filename;
  if axes then Render.render_axes ()

let render shape = Render.render shape
let show shapes = Render.show shapes
