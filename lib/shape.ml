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

(* point - point arithmetic *)
let ( /~ ) p1 p2 = { x = p1.x /. p2.x; y = p1.x /. p2.x }

(* point + scalar arithmetic *)
let ( -! ) { x; y } scalar = { x = x -. scalar; y = y -. scalar }
let ( /! ) { x; y } scalar = { x = x /. scalar; y = y /. scalar }
let point x y = { x; y }

let circle ?point r =
  match point with
  | Some point -> Circle { c = point; radius = r }
  | _ -> Circle { c = { x = 0.; y = 0. }; radius = r }

let make_rectangle c width height =
  let { x; y } = c -! ((width +. height) /. 4.) in
  Polygon
    [
      { x; y };
      { x; y = y +. height };
      { x = x +. width; y = y +. height };
      { x = x +. width; y };
    ]

let rectangle ?point length width =
  make_rectangle
    (match point with Some c -> c | None -> { x = 0.; y = 0. })
    length width

let ellipse ?point rx ry =
  match point with
  | Some c -> Ellipse { c; rx; ry }
  | None -> Ellipse { c = { x = 0.; y = 0. }; rx; ry }

let line ?point point_b =
  match point with
  | Some p -> Line { a = p; b = point_b }
  | _ -> Line { a = { x = 0.; y = 0. }; b = point_b }

let polygon lst_points = Polygon lst_points

let complex shapes =
  match shapes with _ :: _ -> Complex shapes | [] -> Complex []
