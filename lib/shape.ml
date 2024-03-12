type 'a point = { x : 'a; y : 'a }
type color = Color.color
type line = { a : float point; b : float point; stroke : color }
type circle = { c : float point; radius : float; stroke : color; fill : color }

type ellipse = {
  c : float point;
  rx : float;
  ry : float;
  rotation : float;
  stroke : color;
  fill : color;
}

type polygon = { vertices : float point list; stroke : color; fill : color }

type shape =
  | Circle of circle
  | Ellipse of ellipse
  | Line of line
  | Polygon of polygon
  | Complex of shape list

type shapes = shape list

let point x y =
  let x, y = (float_of_int x, float_of_int y) in
  { x; y }

let origin = { x = 0.; y = 0. }

let circle ?(c = origin) r =
  Circle
    {
      c;
      radius = float_of_int r;
      stroke = Color.black;
      fill = Color.transparent;
    }

let polygon vertices =
  Polygon { vertices; stroke = Color.black; fill = Color.transparent }

let rectangle ?(c = origin) width height =
  let w, h = (float_of_int width, float_of_int height) in
  let x = c.x -. (w /. 2.) in
  let y = c.y -. (h /. 2.) in
  polygon
    [
      { x; y }; { x; y = y +. h }; { x = x +. w; y = y +. h }; { x = x +. w; y };
    ]

let ellipse ?(c = origin) rx ry =
  let rx, ry = (float_of_int rx, float_of_int ry) in
  Ellipse
    { c; rx; ry; stroke = Color.black; fill = Color.transparent; rotation = 0. }

let line ?(a = origin) b = Line { a; b; stroke = Color.black }

let complex shapes =
  match shapes with _ :: _ -> Complex shapes | [] -> Complex []

let rec with_stroke stroke = function
  | Circle circle' -> Circle { circle' with stroke }
  | Ellipse ellipse' -> Ellipse { ellipse' with stroke }
  | Line line' -> Line { line' with stroke }
  | Polygon polygon' -> Polygon { polygon' with stroke }
  | Complex complex' -> Complex (List.map (with_stroke stroke) complex')

let rec with_fill fill = function
  | Circle circle' -> Circle { circle' with fill }
  | Ellipse ellipse' -> Ellipse { ellipse' with fill }
  | Polygon polygon' -> Polygon { polygon' with fill }
  | Complex complex' -> Complex (List.map (with_fill fill) complex')
  | _ as line' ->
      print_endline "lines do not have a fill field!";
      line'
