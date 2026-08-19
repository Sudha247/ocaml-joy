open Joy_core

type context = {
  cairo_ctx : Cairo.context;
  surface : Cairo.Surface.t;
  background_color : Color.color;
}

let set_cairo_color ctx (r, g, b, a) =
  let to_float i = float_of_int i /. 255. in
  let r, g, b = Util.tmap3 to_float (r, g, b) in
  Cairo.set_source_rgba ctx.cairo_ctx r g b a

let repaint_background ctx =
  set_cairo_color ctx ctx.background_color;
  let _, _, _, a = ctx.background_color in
  Cairo.paint ctx.cairo_ctx ~alpha:a;
  Cairo.fill ctx.cairo_ctx

let set_line_width ctx line_width =
  Cairo.set_line_width ctx.cairo_ctx (float_of_int line_width)

let draw_circle ctx (cx, cy) radius stroke fill =
  Cairo.arc ctx.cairo_ctx cx (Float.neg cy) ~r:radius ~a1:0. ~a2:(Float.pi *. 2.);
  set_cairo_color ctx stroke;
  Cairo.stroke_preserve ctx.cairo_ctx;
  set_cairo_color ctx fill;
  Cairo.fill_preserve ctx.cairo_ctx;
  Cairo.Path.clear ctx.cairo_ctx

let draw_ellipse ctx (cx, cy) rx ry rotation stroke fill =
  let save_matrix = Cairo.get_matrix ctx.cairo_ctx in
  let radians = Util.to_radians rotation in
  Cairo.rotate ctx.cairo_ctx radians;
  Cairo.translate ctx.cairo_ctx cx (Float.neg cy);
  Cairo.scale ctx.cairo_ctx rx ry;
  Cairo.arc ctx.cairo_ctx 0. 0. ~r:1. ~a1:0. ~a2:(2. *. Float.pi);
  Cairo.set_matrix ctx.cairo_ctx save_matrix;
  set_cairo_color ctx stroke;
  Cairo.stroke_preserve ctx.cairo_ctx;
  set_cairo_color ctx fill;
  Cairo.fill_preserve ctx.cairo_ctx;
  Cairo.Path.clear ctx.cairo_ctx

let draw_line ctx (x1, y1) (x2, y2) stroke =
  set_cairo_color ctx stroke;
  Cairo.move_to ctx.cairo_ctx x1 (Float.neg y1);
  Cairo.line_to ctx.cairo_ctx x2 (Float.neg y2);
  Cairo.stroke ctx.cairo_ctx

let draw_polygon ctx vertices stroke fill =
  let x, y = List.hd vertices in
  let t = List.tl vertices in
  Cairo.move_to ctx.cairo_ctx x (Float.neg y);
  List.iter (fun (x', y') -> Cairo.line_to ctx.cairo_ctx x' (Float.neg y')) t;
  Cairo.Path.close ctx.cairo_ctx;
  set_cairo_color ctx stroke;
  Cairo.stroke_preserve ctx.cairo_ctx;
  set_cairo_color ctx fill;
  Cairo.fill ctx.cairo_ctx

let render ctx shapes =
  let rec paint = function
    | Shape.Circle circle ->
        draw_circle ctx (circle.c.x, circle.c.y) circle.radius circle.stroke
          circle.fill
    | Shape.Ellipse ellipse ->
        draw_ellipse ctx (ellipse.c.x, ellipse.c.y) ellipse.rx ellipse.ry
          ellipse.rotation ellipse.stroke ellipse.fill
    | Shape.Line line ->
        draw_line ctx (line.a.x, line.a.y) (line.b.x, line.b.y) line.stroke
    | Shape.Polygon polygon ->
        let to_tuple (p : Shape.point) = (p.x, p.y) in
        draw_polygon ctx
          (List.map to_tuple polygon.vertices)
          polygon.stroke polygon.fill
    | Shape.Complex complex -> List.iter paint complex
  in
  List.iter paint shapes

let write ctx filename =
  Cairo.PNG.write ctx.surface filename;
  Cairo.Surface.finish ctx.surface

let create ?(background_color = Color.white) ?(line_width = 2)
    ?(size = (500, 500)) () =
  let w, h = size in
  let surface = Cairo.Image.create Cairo.Image.ARGB32 ~w ~h in
  let cairo_ctx = Cairo.create surface in
  Cairo.translate cairo_ctx (w / 2 |> float_of_int) (h / 2 |> float_of_int);
  let ctx = { cairo_ctx; surface; background_color } in
  repaint_background ctx;
  set_line_width ctx line_width;
  ctx
