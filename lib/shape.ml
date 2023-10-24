open Graphics

(* Define types for various geometric shapes and their components *)
type point = { x : int; y : int }
type line = { a : point; b : point }
type rectangle = { c : point; length : int; width : int }
type circle = { c : point; radius : int }
type ellipse = { c : point; rx : int; ry : int }

(* Define a union type 'shape' that represents various shapes *)
type shape =
  | Circle of circle
  | Rectangle of rectangle
  | Ellipse of ellipse
  | Line of line

(* 'shapes' is a list of 'shape' type, representing a collection of shapes *)
type shapes = shape list

(* 'dimensions' is a reference to the canvas dimensions *)
let dimensions = ref { x = 500; y = 500 }
let set_dimensions x y = dimensions := { x; y }

(* Calculate the center of the canvas *)
let canvas_mid = { x = !dimensions.x / 2; y = !dimensions.y / 2 }

(* 'axes_flag' is a reference to control whether axes are drawn *)
let axes_flag = ref false
let draw_axes flag = axes_flag := flag

(* Define a function to draw a line on the canvas *)
let draw_line x1 y1 x2 y2 = draw_poly_line [| (x1, y1); (x2, y2) |]

(* Translate a point from canvas coordinates to screen coordinates *)
let denormalize point =
  { x = point.x + canvas_mid.x; y = point.y + canvas_mid.y }

(* Function to render a shape on the canvas *)
let render_shape s =
  match s with
  | Circle circle ->
      draw_circle (denormalize circle.c).x (denormalize circle.c).y
        circle.radius
  | Rectangle rectangle ->
      let c = denormalize rectangle.c in
      draw_rect c.x c.y rectangle.length rectangle.width
  | Ellipse ellipse ->
      let c = denormalize ellipse.c in
      draw_ellipse c.x c.y ellipse.rx ellipse.ry
  | Line line ->
      let a = denormalize line.a in
      let b = denormalize line.b in
      draw_line a.x a.y b.x b.y

(* Functions to create different shapes with optional position arguments *)
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

(* Function to create a line with optional starting position *)
let line ?x1 ?y1 x2 y2 =
  match (x1, y1) with
  | Some x, Some y -> Line { a = { x; y }; b = { x = x2; y = y2 } }
  | _ -> Line { a = { x = 0; y = 0 }; b = { x = x2; y = y2 } }

(* Translate a shape by a given dx and dy value *)
let translate dx dy shape =
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

(* Scale a shape by a given factor *)
let scale factor s =
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

(* Show a list of shapes on the canvas *)
let show shapes = List.iter render_shape shapes

(* Convert degrees to radians *)
let deg_to_rad degrees = degrees *. (Stdlib.Float.pi /. 180.)

(* Rotate a shape by a specified number of degrees *)
let rotate degrees shape =
  let radians = deg_to_rad degrees in

  (* Convert a point from Cartesian to polar coordinates *)
  let cartesian_to_polar point =
    let r = sqrt (float_of_int (point.x * point.x + point.y * point.y)) in
    let theta = atan2 (float_of_int point.y) (float_of_int point.x) in
    (r, theta)
  in

  (* Convert a point from polar to Cartesian coordinates *)
  let polar_to_cartesian (r, theta) =
    let x = int_of_float (r *. cos theta) in
    let y = int_of_float (r *. sin theta) in
    { x; y }
  in

  match shape with
  | Circle circle ->
      let polar_center = cartesian_to_polar (denormalize circle.c) in
      let polar_rotated = (fst polar_center, snd polar_center +. radians) in
      let rotated_center = polar_to_cartesian polar_rotated in
      (* Make sure the rotated circle remains within the canvas *)
      let max_x = !dimensions.x / 2 in
      let max_y = !dimensions.y / 2 in
      let clamped_x = max (min rotated_center.x max_x) (-max_x) in
      let clamped_y = max (min rotated_center.y max_y) (-max_y) in
      Circle { c = { x = clamped_x; y = clamped_y }; radius = circle.radius }
  | Rectangle rectangle ->
      let polar_center = cartesian_to_polar (denormalize rectangle.c) in
      let polar_rotated = (fst polar_center, snd polar_center +. radians) in
      let rotated_center = polar_to_cartesian polar_rotated in
      (* Make sure the rotated rectangle remains within the canvas *)
      let max_x = !dimensions.x / 2 in
      let max_y = !dimensions.y / 2 in
      let clamped_x = max (min rotated_center.x max_x) (-max_x) in
      let clamped_y = max (min rotated_center.y max_y) (-max_y) in
      Rectangle
        {
          c = { x = clamped_x; y = clamped_y };
          length = rectangle.length;
          width = rectangle.width;
        }
  | Ellipse ellipse ->
      let polar_center = cartesian_to_polar (denormalize ellipse.c) in
      let polar_rotated = (fst polar_center, snd polar_center +. radians) in
      let rotated_center = polar_to_cartesian polar_rotated in
      (* Make sure the rotated ellipse remains within the canvas *)
      let max_x = !dimensions.x / 2 in
      let max_y = !dimensions.y / 2 in
      let clamped_x = max (min rotated_center.x max_x) (-max_x) in
      let clamped_y = max (min rotated_center.y max_y) (-max_y) in
      Ellipse
        {
          c = { x = clamped_x; y = clamped_y };
          rx = ellipse.rx;
          ry = ellipse.ry;
        }
  | Line _line -> failwith "Not Implemented"

(* Compose two functions 'f' and 'g' and apply them to 'x' *)
let compose f g x = g (f x)

(* Render the axes on the canvas *)
let render_axes () =
  set_color (rgb 192 192 192);
  let half_x = size_x () / 2 in
  draw_line half_x 0 half_x (size_y ());
  let half_y = size_y () / 2 in
  draw_line 0 half_y (size_x ()) half_y

(* Initialize the graphics window *)
let init () =
  open_graph (Printf.sprintf " %ix%i" !dimensions.x !dimensions.y);
  if !axes_flag then render_axes ();

  set_color black

(* Close the graphics window after waiting for user input *)
let close () =
  ignore (read_line ());
  close_graph ()