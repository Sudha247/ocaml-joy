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
let ( -! ) { x = x1; y = y1 } scalar = { x = x1 -. scalar; y = y1 -. scalar }

let bi_to_uni { x; y } =
  let cx, cy = Context.resolution () in
  let nx = (x *. 0.5) +. (cx *. 0.5) in
  let ny = (y *. 0.5) +. (cy *. 0.5) in
  (nx, ny)

(* Scales points from 0-image size to 0-1 on both axes *)
let scale_point size point =
  let { x; y } = point in
  let x, y = (x /. fst size, y /. snd size) in
  (x, y)

let denormalize point =
  let x, y = Context.resolution () in
  let canvas_mid = { x; y } /~ { x = 2.; y = 2. } in
  { x = point.x +. canvas_mid.x; y = point.y +. canvas_mid.y }

let point x y = { x; y }

let circle ?point r =
  match point with
  | Some point -> Circle { c = point; radius = r }
  | _ -> Circle { c = { x = 0.; y = 0. }; radius = r }

let make_rectangle c width height =
  let width, height = (width *. 2., height *. 2.) in
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
