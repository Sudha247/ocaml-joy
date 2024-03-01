open Shape
open Context
open Util

let draw_circle ctx ({ c; radius; stroke; fill } : circle) =
  let stroke_circle stroke =
    set_color stroke;
    Cairo.stroke_preserve ctx.ctx
  in
  let fill_circle fill =
    set_color fill;
    Cairo.fill_preserve ctx.ctx
  in
  Cairo.arc ctx.ctx c.x (Float.neg c.y) ~r:radius ~a1:0. ~a2:(Float.pi *. 2.);
  Option.iter stroke_circle stroke;
  Option.iter fill_circle fill;
  Cairo.Path.clear ctx.ctx

let create_control_points ({ x; y }, rx, ry) =
  let half_height = ry /. 2. in
  let width_two_thirds = rx *. (2. /. 3.) *. 2. in
  ( { x; y = y -. half_height },
    ( x +. width_two_thirds,
      y -. half_height,
      x +. width_two_thirds,
      y +. half_height,
      x,
      y +. half_height ),
    ( x -. width_two_thirds,
      y +. half_height,
      x -. width_two_thirds,
      y -. half_height,
      x,
      y -. half_height ) )

let draw_ellipse ctx { c; rx; ry; stroke; fill } =
  let stroke_ellipse stroke =
    set_color stroke;
    Cairo.stroke_preserve ctx.ctx
  in
  let fill_ellipse fill =
    set_color fill;
    Cairo.fill_preserve ctx.ctx
  in
  let start, curve_one, curve_two = create_control_points (c, rx, Float.neg ry) in
  Cairo.move_to ctx.ctx start.x start.y;
  let x1, y1, x2, y2, x3, y3 = curve_one in
  Cairo.curve_to ctx.ctx x1 y1 x2 y2 x3 y3;
  let x1, y1, x2, y2, x3, y3 = curve_two in
  Cairo.curve_to ctx.ctx x1 y1 x2 y2 x3 y3;
  Option.iter stroke_ellipse stroke;
  Option.iter fill_ellipse fill;
  Cairo.Path.clear ctx.ctx

let draw_line ctx { a; b; stroke } =
  set_color stroke;
  let { x; y } = a in
  Cairo.move_to ctx.ctx x (Float.neg y);
  let { x; y } = b in
  Cairo.line_to ctx.ctx x (Float.neg y);
  Cairo.stroke ctx.ctx

let draw_polygon ctx { vertices; stroke; fill } =
  let stroke_rect stroke =
    set_color stroke;
    Cairo.stroke_preserve ctx.ctx
  in
  let fill_rect fill =
    set_color fill;
    Cairo.fill_preserve ctx.ctx
  in
  let points = partition 2 ~step:1 (vertices @ [ List.hd vertices ]) in
  List.iter
    (fun pair ->
      let { x = x1; y = y1 }, { x = x2; y = y2 } =
        (List.nth pair 0, List.nth pair 1)
      in
      Cairo.move_to ctx.ctx x1 (Float.neg y1);
      Cairo.line_to ctx.ctx x2 (Float.neg y2))
    points;
  Option.iter stroke_rect stroke;
  Option.iter fill_rect fill;
  Cairo.Path.clear ctx.ctx


(* Validates context before rendering *)
let show shapes =
  let rec render ctx = function
    | Circle circle -> draw_circle ctx circle
    | Ellipse ellipse -> draw_ellipse ctx ellipse
    | Line line -> draw_line ctx line
    | Polygon polygon -> draw_polygon ctx polygon
    | Complex complex -> List.iter (render ctx) complex
  in
  match !context with
  | Some ctx -> List.iter (render ctx) shapes
  | None -> fail ()

let render_axes () =
  let x, y = resolution () |> tmap float_of_int in
  let half_x, half_y = (x /. 2., y /. 2.) in
  let x_axis = line ~a:{ x = 0.; y = -.half_y } { x = 0.; y = half_y } in
  let y_axis = line ~a:{ x = -.half_x; y = 0. } { x = half_x; y = 0. } in
  show [ x_axis; y_axis ]
