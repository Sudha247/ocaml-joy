type context = {
  cairo_ctx : Cairo.context;
  surface : Cairo.Surface.t;
  size : int * int;
  background_color : int * int * int * float;
  axes : bool;
}

let write ctx filename =
  Cairo.PNG.write ctx.surface filename;
  Cairo.Surface.finish ctx.surface

let set_color ctx color =
  let to_float i = float_of_int i /. 255. in
  let r, g, b, a = color in
  let r, g, b = Util.tmap3 to_float (r, g, b) in
  Cairo.set_source_rgba ctx.cairo_ctx r g b a

let set_background ctx color =
  let r, g, b, a = color in
  let to_float i = float_of_int i /. 255. in
  let r, g, b = Util.tmap3 to_float (r, g, b) in
  Cairo.set_source_rgba ctx.cairo_ctx r g b a;
  Cairo.paint ctx.cairo_ctx ~alpha:a;
  Cairo.fill ctx.cairo_ctx

(** Sets the width of lines for both stroke of shapes and line primitives. 
    Can be any positive integer, with larger numbers producing thicker lines. *)
let set_line_width ctx line_width =
  Cairo.set_line_width ctx.cairo_ctx (float_of_int line_width)

let draw_circle ctx (cx, cy) radius stroke fill =
  Cairo.arc ctx.cairo_ctx cx (Float.neg cy) ~r:radius ~a1:0. ~a2:(Float.pi *. 2.);
  set_color ctx stroke;
  Cairo.stroke_preserve ctx.cairo_ctx;
  set_color ctx fill;
  Cairo.fill_preserve ctx.cairo_ctx;
  Cairo.Path.clear ctx.cairo_ctx

let draw_ellipse ctx (cx, cy) rx ry stroke fill =
  (* Save the current transformation matrix *)
  let save_matrix = Cairo.get_matrix ctx.cairo_ctx in

  (* Translate and scale to create an ellipse from a circle *)
  Cairo.translate ctx.cairo_ctx cx (Float.neg cy);
  Cairo.scale ctx.cairo_ctx rx ry;
  Cairo.arc ctx.cairo_ctx 0. 0. ~r:1. ~a1:0. ~a2:(2. *. Float.pi);

  (* Restore the original transformation matrix *)
  Cairo.set_matrix ctx.cairo_ctx save_matrix;

  set_color ctx stroke;
  Cairo.stroke_preserve ctx.cairo_ctx;
  set_color ctx fill;
  Cairo.fill_preserve ctx.cairo_ctx;

  Cairo.Path.clear ctx.cairo_ctx

let draw_line ctx (x1, y1) (x2, y2) stroke =
  set_color ctx stroke;
  Cairo.move_to ctx.cairo_ctx x1 (Float.neg y1);
  Cairo.line_to ctx.cairo_ctx x2 (Float.neg y2);
  Cairo.stroke ctx.cairo_ctx

let draw_polygon ctx vertices stroke fill =
  let x, y = List.hd vertices in
  let t = List.tl vertices in
  Cairo.move_to ctx.cairo_ctx x (Float.neg y);
  List.iter (fun (x', y') -> Cairo.line_to ctx.cairo_ctx x' (Float.neg y')) t;
  Cairo.Path.close ctx.cairo_ctx;
  set_color ctx stroke;
  Cairo.stroke_preserve ctx.cairo_ctx;
  set_color ctx fill;
  Cairo.fill ctx.cairo_ctx

let show ctx shapes =
  let rec render = function
    | Shape.Circle circle ->
        draw_circle ctx (circle.c.x, circle.c.y) circle.radius circle.stroke
          circle.fill
    | Shape.Ellipse ellipse ->
        draw_ellipse ctx (ellipse.c.x, ellipse.c.y) ellipse.rx ellipse.ry
          ellipse.stroke ellipse.fill
    | Shape.Line line ->
        draw_line ctx (line.a.x, line.a.y) (line.b.x, line.b.y) line.stroke
    | Shape.Polygon polygon ->
        let to_tuple (point : float Shape.point) = (point.x, point.y) in
        draw_polygon ctx
          (List.map to_tuple polygon.vertices)
          polygon.stroke polygon.fill
    | Shape.Complex complex -> List.iter render complex
  in
  List.iter render shapes

let create ~background_color ~line_width ~size ~axes =
  let w, h = size in
  let surface = Cairo.Image.create Cairo.Image.ARGB32 ~w ~h in
  let cairo_ctx = Cairo.create surface in
  Cairo.translate cairo_ctx (w / 2 |> float_of_int) (h / 2 |> float_of_int);
  let ctx = { cairo_ctx; surface; size = (w, h); background_color; axes } in
  set_background ctx background_color;
  set_line_width ctx line_width;
  ctx
