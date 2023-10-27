open Graphics

type point = { x : int; y : int }
type line = { a : point; b : point }
type circle = { c : point; radius : int }
type ellipse = { c : point; rx : int; ry : int }
type polygon = point list

type shape =
  | Circle of circle
  | Ellipse of ellipse
  | Line of line
  | Polygon of polygon
  | Complex of shape list

type shapes = shape list

let dimensions = ref { x = 500; y = 500 }
let set_dimensions x y = dimensions := { x; y }
let canvas_mid = { x = !dimensions.x / 2; y = !dimensions.y / 2 }
let axes_flag = ref false
let draw_axes flag = axes_flag := flag
let draw_line x1 y1 x2 y2 = draw_poly_line [| (x1, y1); (x2, y2) |]

let bi_to_uni x y =
  let nx = (x *. 0.5) +. (float_of_int !dimensions.x *. 0.5) in
  let ny = (y *. 0.5) +. (float_of_int !dimensions.y *. 0.5) in
  (int_of_float nx, int_of_float ny)

let denormalize point =
  { x = point.x + canvas_mid.x; y = point.y + canvas_mid.y }

let render_polygon polygon' =
  let points =
    Array.of_list
      (List.map
         (fun point ->
           let { x; y } = denormalize point in
           (x, y))
         polygon')
  in
  draw_poly points

let rec render_shape s =
  match s with
  | Circle circle ->
      draw_circle (denormalize circle.c).x (denormalize circle.c).y
        circle.radius
  | Ellipse ellipse ->
      let c = denormalize ellipse.c in
      draw_ellipse c.x c.y ellipse.rx ellipse.ry
  | Line line ->
      let a = denormalize line.a in
      let b = denormalize line.b in
      draw_line a.x a.y b.x b.y
  | Polygon polygon' -> render_polygon polygon'
  | Complex complex -> List.iter render_shape complex

let point x y = { x; y }

let circle ?point r =
  match point with
  | Some point -> Circle { c = point; radius = r }
  | _ -> Circle { c = { x = 0; y = 0 }; radius = r }

let rectangle ?point length width =
  let x, y = match point with Some { x; y } -> (x, y) | None -> (0, 0) in
  Polygon
    [
      { x; y };
      { x; y = y + length };
      { x = x + width; y = y + length };
      { x = x + width; y };
    ]

let ellipse ?point rx ry =
  match point with
  | Some point -> Ellipse { c = point; rx; ry }
  | _ -> Ellipse { c = { x = 0; y = 0 }; rx; ry }

let line ?point_a point_b =
  match point_a with
  | Some point_a -> Line { a = point_a; b = point_b }
  | _ -> Line { a = { x = 0; y = 0 }; b = point_b }

let polygon lst_points = Polygon lst_points

let complex shapes =
  match shapes with _ :: _ -> Complex shapes | [] -> Complex []

let rec translate dx dy shape =
  match shape with
  | Circle circle ->
      Circle { circle with c = { x = circle.c.x + dx; y = circle.c.y + dy } }
  | Ellipse ellipse ->
      Ellipse
        { ellipse with c = { x = ellipse.c.x + dx; y = ellipse.c.y + dy } }
  | Line line ->
      Line
        {
          a = { x = line.a.x + dx; y = line.a.y + dy };
          b = { x = line.b.x + dx; y = line.b.y + dy };
        }
  | Polygon polygon' ->
      polygon
        (List.map
           (fun { x : int; y : int } -> { x = x + dx; y = y + dy })
           polygon')
  | Complex shapes -> Complex (List.map (translate dx dy) shapes)

let rec scale factor s =
  let round x = int_of_float (x +. 0.5) in
  let scale_length len fact = round (float_of_int len *. sqrt fact) in
  match s with
  | Circle circle' ->
      circle ~point:circle'.c (scale_length circle'.radius factor)
  | Ellipse ellipse' ->
      ellipse ~point:ellipse'.c
        (scale_length ellipse'.rx factor)
        (scale_length ellipse'.ry factor)
  | Line _line' -> failwith "Not Implemented"
  | Polygon _polygon' -> failwith "Scale not implemeted for polygons"
  | Complex shapes -> Complex (List.map (scale factor) shapes)

let show shapes = List.iter render_shape shapes
let deg_to_rad degrees = degrees *. (Stdlib.Float.pi /. 180.)

let rot { x : int; y : int } degrees =
  let radians = deg_to_rad (float_of_int degrees) in
  let dx = (float_of_int x *. cos radians) -. (float_of_int y *. sin radians) in
  let dy = (float_of_int x *. sin radians) +. (float_of_int y *. cos radians) in
  let dx, dy = bi_to_uni dx dy in
  { x = dx; y = dy }

let rec rotate degrees shape =
  match shape with
  | Circle circle -> Circle { c = rot circle.c degrees; radius = circle.radius }
  | Ellipse ellipse ->
      Ellipse { c = rot ellipse.c degrees; rx = ellipse.rx; ry = ellipse.ry }
  | Line _line -> failwith "Not Implemented"
  | Polygon polygon' -> polygon (List.map (fun p -> rot p degrees) polygon')
  | Complex shapes -> Complex (List.map (rotate degrees) shapes)

let compose f g x = g (f x)
let rec range a b = if a >= b then [] else a :: range (a + 1) b

let repeat n op shape =
  let match_list l =
    match l with [] -> [ op shape ] | last :: _ -> op last :: l
  in
  let shapes = List.fold_right (fun _ acc -> match_list acc) (range 0 n) [] in
  complex shapes

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
