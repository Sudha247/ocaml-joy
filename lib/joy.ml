include Shape
include Transform
include Color

let context = Context.context

let set_line_width = Context.set_line_width

let init ?(background = Color.white) ?(line_width = 2) ?(size = (500, 500))
    ?(axes = false) () =
  Context.init_context (Color.opaque background) (float_of_int line_width) size
    axes

let write ?(filename = "joy.png") () =
  match !Context.context with
  | Some ctx ->
      if ctx.axes then Render.render_axes ();
      Context.write ctx filename
  | None -> Context.fail ()

let show shapes = Render.show shapes
