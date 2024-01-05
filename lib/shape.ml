type point = { x : float; y : float }
type line = { a : point; b : point }
type circle = { c : point; radius : float }
type ellipse = { c : point; rx : float; ry : float }
type polygon = point list

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
let ( *! ) {x; y} scalar = {x = x *. scalar; y = y *. scalar}
let point x y = { x; y }

let center = {x= 0.; y = 0.}

let circle ?(point = center) r =
  Circle { c = point; radius = r }

let rectangle ?(point = center) width height =
  let { x; y } = point -! ((width +. height) /. 4.) in
  Polygon
    [
      { x; y };
      { x; y = y +. height };
      { x = x +. width; y = y +. height };
      { x = x +. width; y };
    ]
  

let ellipse ?(point = center) rx ry =
  Ellipse { c = point; rx; ry }

let line ?(point = center) point_b =
  Line { a = point; b = point_b }

let polygon lst_points = Polygon lst_points

let complex shapes =
  match shapes with _ :: _ -> Complex shapes | [] -> Complex []
