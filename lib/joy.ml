include Random
include Shape
include Transform
include Color

module Backend_cairo = Backend_cairo
module Backend_svg = Backend_svg
module Backend_lazy = Backend_lazy

type context = Context.context
let show = Context.show
let set_line_width = Context.set_line_width

(* let init ?(background = Color.white) ?(line_width = 2) ?(size = (500, 500))
    ?(axes = false) () =
  Context.init_context (Color.opaque background) (float_of_int line_width) size
    axes *)

let init ?(size = (500, 500)) ?(line_width = 2) ?(axes = false) _ =
  let ctx = Backend_cairo.create ~background_color:Color.white ~size ~line_width ~axes in
  let ctx_container = Context.CairoContext ctx in
  Context.set_default ctx_container;
  if axes then
    let half_w, half_h = ctx.size |> Util.tmap float_of_int |> Util.tmap ((/.) 2.) in
    let x_axis = line ~a:{x = -.half_w; y = 0.} {x = half_w; y = 0.} in
    let y_axis = line ~a:{x = 0.; y = -.half_h} {x = 0.; y = half_h} in
    show ~ctx:ctx_container [x_axis; y_axis]

let write ?(filename = "joy.png") () =
  Context.writePNG filename

