open Shape
open Context

let tmap f (x, y) = (f x, f y)

let denormalize point =
  let x, y = Context.resolution () |> tmap float_of_int in
  let canvas_mid = { x; y } /! 2. in
  ((point.x +. canvas_mid.x) /. x, (point.y +. canvas_mid.y) /. y)

let euclid_norm (x, y) = sqrt (Float.pow x 2. +. Float.pow y 2.) /. 2.

let draw_circle ctx ({ c; radius } : circle) =
  let size = tmap float_of_int ctx.size in
  let x, y = denormalize c in
  let radius = radius /. euclid_norm size in
  Cairo.arc ctx.ctx x y ~r:radius ~a1:0. ~a2:(Float.pi *. 2.);
  Cairo.stroke ctx.ctx

let create_control_points { c; rx; ry } =
  let size = resolution () |> tmap float_of_int in
  let x, y = denormalize c in
  let half_height = ry /. snd size in
  let width_two_thirds = rx /. fst size *. (2. /. 3.) *. 2. in
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

let draw_ellipse ctx ellipse =
  let start, curve_one, curve_two = create_control_points ellipse in
  Cairo.save ctx.ctx;
  Cairo.move_to ctx.ctx start.x start.y;
  let x1, y1, x2, y2, x3, y3 = curve_one in
  Cairo.curve_to ctx.ctx x1 y1 x2 y2 x3 y3;
  let x1, y1, x2, y2, x3, y3 = curve_two in
  Cairo.curve_to ctx.ctx x1 y1 x2 y2 x3 y3;
  Cairo.stroke ctx.ctx;
  Cairo.restore ctx.ctx

let draw_line ctx line =
  save ();
  let x1, y1 = denormalize line.a in
  let x2, y2 = denormalize line.b in
  Cairo.move_to ctx.ctx x1 y1;
  Cairo.line_to ctx.ctx x2 y2;
  Cairo.stroke ctx.ctx;
  restore ()

let rec take n lst =
  match (n, lst) with
  | 0, _ -> ([], lst)
  | _, [] -> ([], [])
  | n, x :: xs ->
      let taken, rest = take (n - 1) xs in
      (x :: taken, rest)

let rec partition n ?step lst =
  match lst with
  | [] -> []
  | _ ->
      let taken, _ = take n lst in
      if List.length taken = n then
        taken
        ::
        (match step with
        | Some s -> partition n ~step:s (List.tl lst)
        | None -> partition n ~step:0 (List.tl lst))
      else []

let draw_polygon ctx polygon =
  let points = partition 2 ~step:1 (polygon @ [ List.hd polygon ]) in
  List.iter
    (fun pair ->
      let pair = List.map denormalize pair in
      let (x1, y1), (x2, y2) = (List.nth pair 0, List.nth pair 1) in
      Cairo.move_to ctx.ctx x1 y1;
      Cairo.line_to ctx.ctx x2 y2)
    points;
  Cairo.move_to ctx.ctx 0. 0.;
  Cairo.stroke ctx.ctx

let rec render_shape ctx = function
  | Circle circle -> draw_circle ctx circle
  | Ellipse ellipse -> draw_ellipse ctx ellipse
  | Line line -> draw_line ctx line
  | Polygon polygon -> draw_polygon ctx polygon
  | Complex complex -> List.iter (render_shape ctx) complex

(* Validates context before rendering *)
let render shape =
  match !context with Some ctx -> render_shape ctx shape | None -> fail ()

let show shapes =
  match !context with
  | Some ctx -> List.iter (render_shape ctx) shapes
  | None -> fail ()

let render_axes () =
  print_endline "rendering axes!";
  save ();
  let x, y = Context.resolution () |> tmap float_of_int in
  let half_x, half_y = (x /. 2., y /. 2.) in
  let x_axis = line ~a:{ x = 0.; y = -.half_y } { x = 0.; y = half_y } in
  let y_axis = line ~a:{ x = -.half_x; y = 0. } { x = half_x; y = 0. } in
  set_color (0, 0, 0);
  show [ x_axis; y_axis ];
  restore ()
