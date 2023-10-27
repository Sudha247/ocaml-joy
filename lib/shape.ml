open Graphics

type point = { x : int; y : int }
type line = { a : point; b : point }
type rectangle = { c : point; length : int; width : int }
type circle = { c : point; radius : int }
type ellipse = { c : point; rx : int; ry : int }

type shape =
  | Circle of circle
  | Rectangle of rectangle
  | Ellipse of ellipse
  | Line of line
  | Complex of shape list

type shapes = shape list

let dimensions = ref { x = 500; y = 500 }
let set_dimensions x y = dimensions := { x; y }

let canvas_mid = { x = !dimensions.x / 2; y = !dimensions.y / 2 }

let axes_flag = ref false
let draw_axes flag = axes_flag := flag

let draw_line x1 y1 x2 y2 = draw_poly_line [| (x1, y1); (x2, y2) |]

let denormalize point =
  { x = point.x + canvas_mid.x; y = point.y + canvas_mid.y }

let deg_to_rad degrees = degrees *. (Float.pi /. 180.)

let rotate_point point radians =
  let cartesian_to_polar { x; y } =
    let r = sqrt (float_of_int (x * x + y * y)) in
    let theta = atan2 (float_of_int y) (float_of_int x) in
    (r, theta)
  in

  let polar_to_cartesian (r, theta) =
    let x = int_of_float (r *. cos theta) in
    let y = int_of_float (r *. sin theta) in
    { x; y }
  in

  match point with
  | { x = x'; y = y' } ->
    let (r, theta) = cartesian_to_polar point in
    let { x; y } = polar_to_cartesian (r, theta +. radians) in
    { x = x + x'; y = y + y' }

let rec render_shape s =
  match s with
  | Circle circle ->
    let rotated_center = rotate_point circle.c (deg_to_rad 30.0) in
    draw_circle (denormalize rotated_center).x (denormalize rotated_center).y circle.radius
  | Rectangle rectangle ->
    let rotated_center = rotate_point rectangle.c (deg_to_rad 30.0) in
    let c = denormalize rotated_center in
    draw_rect c.x c.y rectangle.length rectangle.width
  | Ellipse ellipse ->
    let rotated_center = rotate_point ellipse.c (deg_to_rad 30.0) in
    let c = denormalize rotated_center in
    draw_ellipse c.x c.y ellipse.rx ellipse.ry
  | Line line ->
    let a = denormalize (rotate_point line.a (deg_to_rad 30.0)) in
    let b = denormalize (rotate_point line.b (deg_to_rad 30.0)) in
    draw_line a.x a.y b.x b.y
  | Complex complex -> List.iter render_shape complex

let circle ?x ?y r =
  match (x, y) with
  | Some x, Some y -> Circle { c = { x; y }; radius = r }
  | _ -> Circle { c = { x = 0; y = 0 }; radius = r }

let rectangle ?x ?y length width =
  match (x, y) with
  | Some x, Some y -> Rectangle { c = { x; y }; length; width }
  | _ -> Rectangle { c = { x = 0; y = 0 }; length; width }

let ellipse ?x ?y rx ry =
  match (x, y) with
  | Some x, Some y -> Ellipse { c = { x; y }; rx; ry }
  | _ -> Ellipse { c = { x = 0; y = 0 }; rx; ry }

let line ?x1 ?y1 x2 y2 =
  match (x1, y1) with
  | Some x, Some y -> Line { a = { x; y }; b = { x = x2; y = y2 } }
  | _ -> Line { a = { x = 0; y = 0 }; b = { x = x2; y = y2 } }

let complex shapes =
  match shapes with _ :: _ -> Complex shapes | [] -> Complex []

let rec translate dx dy shape =
  match shape with
  | Circle circle ->
    Circle { circle with c = { x = circle.c.x + dx; y = circle.c.y + dy } }
  | Rectangle rectangle ->
    Rectangle
      {
        rectangle with
        c = { x = rectangle.c.x + dx; y = rectangle.c.y + dy };
      }
  | Ellipse ellipse ->
    Ellipse
      { ellipse with c = { x = ellipse.c.x + dx; y = ellipse.c.y + dy } }
  | Line line ->
      Line
        {
          a = { x = line.a.x + dx; y = line.a.y + dy };
          b = { x = line.b.x + dx; y = line.b.y + dy };
        }
  | Complex shapes -> Complex (List.map (translate dx dy) shapes)

let rec scale factor s =
  let round x = int_of_float (x +. 0.5) in
  let scale_length len fact = round (float_of_int len *. sqrt fact) in
  match s with
  | Circle circle' ->
    circle ~x:circle'.c.x ~y:circle'.c.y (scale_length circle'.radius factor)
  | Rectangle rectangle' ->
    rectangle ~x:rectangle'.c.x ~y:rectangle'.c.y
      (scale_length rectangle'.length factor)
      (scale_length rectangle'.width factor)
  | Ellipse ellipse' ->
    ellipse ~x:ellipse'.c.x ~y:ellipse'.c.y
      (scale_length ellipse'.rx factor)
      (scale_length ellipse'.ry factor)
  | Line _line' -> failwith "Not Implemented"
  | Complex shapes -> Complex (List.map (scale factor) shapes)

let show shapes = List.iter render_shape shapes

let deg_to_rad degrees = degrees *. (Stdlib.Float.pi /. 180.)

let rec rotate degrees shape =
  match shape with
  | Circle circle ->
    let rotated_center = rotate_point circle.c (deg_to_rad (float_of_int degrees)) in
    Circle { c = rotated_center; radius = circle.radius }
  | Rectangle rectangle ->
    let rotated_center = rotate_point rectangle.c (deg_to_rad (float_of_int degrees)) in
    Rectangle
      {
        c = rotated_center;
        length = rectangle.length;
        width = rectangle.width;
      }
  | Ellipse ellipse ->
    let rotated_center = rotate_point ellipse.c (deg_to_rad (float_of_int degrees)) in
    Ellipse
      {
        c = rotated_center;
        rx = ellipse.rx;
        ry = ellipse.ry;
      }
  | Line _line -> failwith "Not Implemented"
  | Complex shapes -> Complex (List.map (rotate degrees) shapes)

let compose f g x = g (f x)

let render_axes () =
  set_color (rgb 192 192 192);
  let half_x = size_x () / 2 in
  draw_line half_x 0 half_x (size_y ());
  let half_y = size_y () / 2 in
  draw_line 0 half_y (size_x ()) half_y

let init () =
  open_graph (Printf.sprintf " %ix%i" !dimensions.x !dimensions.y);
  if !axes_flag then render_axes ();

  set_color black

let close () =
  ignore (read_line ());
  close_graph ()