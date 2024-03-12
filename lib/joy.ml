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

let init ?(size = (500, 500)) ?(line_width = 2) ?(axes = false) _ =
  let ctx = Backend_cairo.create ~background_color:Color.white ~size ~line_width ~axes in
  let ctx_container = Context.CairoContext ctx in
  Context.set_default ctx_container;
  if axes then
    let half_w, half_h = ctx.size |> Util.tmap float_of_int |> Util.tmap (fun x -> x /. 2.0) in
    let gray = Color.color 128 128 128 ~a:0.5 in
    let x_axis = line ~a:{x = -.half_w; y = 0.} {x = half_w; y = 0.} |> with_stroke gray in
    let y_axis = line ~a:{x = 0.; y = -.half_h} {x = 0.; y = half_h} |> with_stroke gray in
    show ~ctx:ctx_container [x_axis; y_axis]

let write ?(filename = "joy.png") () =
  Context.writePNG filename

