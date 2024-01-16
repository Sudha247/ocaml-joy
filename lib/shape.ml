type 'a point = { x : 'a; y : 'a }
type line = { a : float point; b : float point }
type circle = { c : float point; radius : float }
type ellipse = { c : float point; rx : float; ry : float }
type polygon = float point list

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
  let width, height = (float_of_int width, float_of_int height) in
  let { x; y } = c -! ((width +. height) /. 4.) in
  Polygon
    [
      { x; y };
      { x; y = y +. height };
      { x = x +. width; y = y +. height };
      { x = x +. width; y };
    ]

let ellipse ?(c = center) rx ry =
  let rx, ry = (float_of_int rx, float_of_int ry) in
  Ellipse { c; rx; ry }

let line ?(a = center) b = Line { a; b }
let polygon lst_points = Polygon lst_points

let complex shapes =
  match shapes with _ :: _ -> Complex shapes | [] -> Complex []
