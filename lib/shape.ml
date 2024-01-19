type point = { x : float; y : float }
type line = { a : point; b : point; color: Color.color }
type circle = { c : point; radius : float; color: Color.color }
type ellipse = { c : point; rx : float; ry : float; color: Color.color }
type polygon = { vertices: point list; color: Color.color}

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
let circle ?(c = center) r = Circle { c; radius = float_of_int r }

let rectangle ?(c = center) width height =
  let w, h = (float_of_int width, float_of_int height) in
  let x1 = c.x -. (w /. 2.) in
  let y1 = c.y -. (h /. 2.) in
  Polygon
    [
      { x = x1; y = y1 };
      { x = x1; y = y1 +. h };
      { x = x1 +. w; y = y1 +. h };
      { x = x1 +. w; y = y1 };
    ]

let ellipse ?(c = center) rx ry =
  let rx, ry = (float_of_int rx, float_of_int ry) in
  Ellipse { c; rx; ry }

let line ?(a = center) b = Line { a; b }
let polygon lst_points = Polygon lst_points

let complex shapes =
  match shapes with _ :: _ -> Complex shapes | [] -> Complex []

let rec with_color color = function 
| Circle circle -> Circle { circle with color = color }
| Ellipse ellipse -> Ellipse { ellipse with color = color }
| Line line -> Line { line with color = color }
| Polygon polygon' -> Polygon { polygon' with color = color } 
| Complex complex' -> Complex (List.map (with_color color) complex')