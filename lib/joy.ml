include Shape
include Transform
include Color

module Context = Context
module Backend_cairo = Backend_cairo
module Backend_svg = Backend_svg
module Backend_lazy = Backend_lazy

let random = Random.random
let frandom = Random.frandom
let fractal_noise = Random.fractal_noise

type context = Context.context

let show = Context.show
let clear = Context.clear
let set_line_width = Context.set_line_width

let init ?(size = (500, 500)) ?(line_width = 1) ?(axes = false) _ =
  let ctx =
    Backend_cairo.create ~background_color:Color.white ~size ~line_width ~axes
  in
  let ctx_container = Context.CairoContext ctx in
  Context.set_default ctx_container;
  if axes then
    let half_w, half_h =
      ctx.size |> Util.tmap float_of_int |> Util.tmap (fun x -> x /. 2.0)
    in
    let gray = Color.color 128 128 128 ~a:0.5 in
    let x_axis =
      line ~a:{ x = -.half_w; y = 0. } { x = half_w; y = 0. }
      |> with_stroke gray
    in
    let y_axis =
      line ~a:{ x = 0.; y = -.half_h } { x = 0.; y = half_h }
      |> with_stroke gray
    in
    show ~ctx:ctx_container [ x_axis; y_axis ]

let init_svg ?(size = (500, 500)) ?(axes = false) eltId =
  let ctx = Backend_svg.create ~size ~axes ~eltId in
  let ctx_container = Context.SVGContext ctx in
  Context.set_default ctx_container;
  if axes then
    let half_w, half_h =
      ctx.size |> Util.tmap float_of_int |> Util.tmap (fun x -> x /. 2.0)
    in
    let gray = Color.color 128 128 128 ~a:0.5 in
    let x_axis =
      line ~a:{ x = -.half_w; y = 0. } { x = half_w; y = 0. }
      |> with_stroke gray
    in
    let y_axis =
      line ~a:{ x = 0.; y = -.half_h } { x = 0.; y = half_h }
      |> with_stroke gray
    in
    show ~ctx:ctx_container [ x_axis; y_axis ]

let write ?(filename = "joy.png") () = Context.writePNG filename
