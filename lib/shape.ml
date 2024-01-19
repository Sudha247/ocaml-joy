type 'a point = { x : 'a; y : 'a }
type line = { a : float point; b : float point; color: Color.color }
type circle = { c : float point; radius : float; color: Color.color }
type ellipse = { c : float point; rx : float; ry : float; color: Color.color }
type polygon = { vertices: float point list; color: Color.color}

type shape =
  | Circle of circle
  | Ellipse of ellipse
  | Line of line
  | Polygon of polygon
  | Complex of shape list

type shapes = shape list

type color = Color.color

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
let circle ?(c = center) r = Circle { c; radius = float_of_int r; color = Color.black }

let polygon vertices = Polygon { vertices; color = Color.black }
let rectangle ?(c = center) width height =
  let w, h = (float_of_int width, float_of_int height) in
  let x1 = c.x -. (w /. 2.) in
  let y1 = c.x -. (h /. 2.) in
  polygon
    [
      { x = x1; y = y1 };
      { x = x1; y = y1 +. h };
      { x = x1 +. w; y = y1 +. h };
      { x = x1 +. w; y = y1 };
    ]

let ellipse ?(c = center) rx ry =
  let rx, ry = (float_of_int rx, float_of_int ry) in
  Ellipse { c; rx; ry; color = Color.black }

let line ?(a = center) b = Line { a; b; color = Color.black }


let complex shapes =
  match shapes with _ :: _ -> Complex shapes | [] -> Complex []

let rec with_stroke color = function
  | Circle circle' -> Circle { circle' with color }
  | Ellipse ellipse' -> Ellipse { ellipse' with color }
  | Line line' -> Line { line' with color }
  | Polygon polygon' -> Polygon { polygon' with color }
  | Complex complex' -> Complex (List.map (with_stroke color) complex')
