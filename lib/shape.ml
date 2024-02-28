type 'a point = { x : 'a; y : 'a }
(** A point in 2d space *)

type color = Color.color
type line = { a : float point; b : float point; stroke : color }

type circle = {
  c : float point;
  radius : float;
  stroke : color option;
  fill : color option;
}

type ellipse = {
  c : float point;
  rx : float;
  ry : float;
  stroke : color option;
  fill : color option;
}

type polygon = {
  vertices : float point list;
  stroke : color option;
  fill : color option;
}

type shape =
  | Circle of circle
  | Ellipse of ellipse
  | Line of line
  | Polygon of polygon
  | Complex of shape list

type shapes = shape list

(* point -> point arithmetic *)
let ( /~ ) p1 p2 = { x = p1.x /. p2.x; y = p1.x /. p2.x }

(* point -> scalar arithmetic *)
let ( -! ) { x; y } scalar = { x = x -. scalar; y = y -. scalar }
let ( /! ) { x; y } scalar = { x = x /. scalar; y = y /. scalar }
let ( *! ) { x; y } scalar = { x = x *. scalar; y = y *. scalar }
let pmap f { x; y } = { x = f x; y = f y }

let point x y =
  let x, y = (float_of_int x, float_of_int y) in
  { x; y }

let center = { x = 0.; y = 0. }

let circle ?(c = center) r =
  Circle { c; radius = float_of_int r; stroke = Some Color.black; fill = None }

let polygon vertices =
  Polygon { vertices; stroke = Some Color.black; fill = None }

let rectangle ?(c = center) width height =
  let w, h = (float_of_int width, float_of_int height) in
  let x1 = c.x -. (w /. 2.) in
  let y1 = c.y -. (h /. 2.) in
  polygon
    [
      { x = x1; y = y1 };
      { x = x1; y = y1 +. h };
      { x = x1 +. w; y = y1 +. h };
      { x = x1 +. w; y = y1 };
    ]

let ellipse ?(c = center) rx ry =
  let rx, ry = (float_of_int rx, float_of_int ry) in
  Ellipse { c; rx; ry; stroke = Some Color.black; fill = None }

let line ?(a = center) b = Line { a; b; stroke = Color.black }

let complex shapes =
  match shapes with _ :: _ -> Complex shapes | [] -> Complex []

let rec with_stroke stroke = function
  | Circle circle' -> Circle { circle' with stroke = Some stroke }
  | Ellipse ellipse' -> Ellipse { ellipse' with stroke = Some stroke }
  | Line line' -> Line { line' with stroke }
  | Polygon polygon' -> Polygon { polygon' with stroke = Some stroke }
  | Complex complex' -> Complex (List.map (with_stroke stroke) complex')

let rec with_fill fill = function
  | Circle circle' -> Circle { circle' with fill = Some fill }
  | Ellipse ellipse' -> Ellipse { ellipse' with fill = Some fill }
  | Polygon polygon' -> Polygon { polygon' with fill = Some fill }
  | Complex complex' -> Complex (List.map (with_fill fill) complex')
  | _ as line' ->
      print_endline "lines do not have a fill field!";
      line'
