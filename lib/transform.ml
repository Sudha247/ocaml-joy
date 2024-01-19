open Shape

type transformation = shape -> shape

let rec translate dx dy = function
  | Circle circle ->
      let dx, dy = (float_of_int dx, float_of_int dy) in
      Circle { circle with c = { x = circle.c.x +. dx; y = circle.c.y +. dy } }
  | Ellipse ellipse ->
      let dx, dy = (float_of_int dx, float_of_int dy) in
      Ellipse
        { ellipse with c = { x = ellipse.c.x +. dx; y = ellipse.c.y +. dy } }
  | Line line' ->
      let dx, dy = (float_of_int dx, float_of_int dy) in
      Line
        {
          line' with
          a = { x = line'.a.x +. dx; y = line'.a.y +. dy };
          b = { x = line'.b.x +. dx; y = line'.b.y +. dy };
        }
  | Polygon polygon' ->
      Polygon
        {
          polygon' with
          vertices =
            List.map
              (fun { x; y } -> { x = x +. float_of_int dx; y = y +. float_of_int dy })
              polygon'.vertices;
        }
  | Complex shapes -> Complex (List.map (translate dx dy) shapes)

let scale_length fact len = len *. fact
let pmap f { x; y } = { x = f x; y = f y }

let rec scale factor = function
  | Circle circle' ->
      Circle
        {
          circle' with
          c = pmap (scale_length factor) circle'.c;
          radius = scale_length factor circle'.radius;
        }
  | Ellipse ellipse' ->
      Ellipse
        {
          ellipse' with
          c = pmap (scale_length factor) ellipse'.c;
          rx = scale_length factor ellipse'.rx;
          ry = scale_length factor ellipse'.ry;
        }
  | Line _line' -> failwith "Not Implemented"
  | Polygon polygon' ->
      Polygon
        {
          polygon' with
          vertices = List.map (pmap (scale_length factor)) polygon'.vertices;
        }
  | Complex shapes -> Complex (List.map (scale factor) shapes)

let to_radians degrees = float_of_int degrees *. Stdlib.Float.pi /. 180.

let to_polar point =
  let { x; y } = point in
  (sqrt ((x *. x) +. (y *. y)), atan2 y x)

let from_polar polar_point =
  let r, theta = polar_point in
  { x = r *. cos theta; y = r *. sin theta }

let rotate_point degrees point =
  let radians = to_radians degrees in
  let r, theta = to_polar point in
  from_polar (r, theta +. radians)

let rec rotate degrees = function
  | Circle circle' -> Circle { circle' with c = rotate_point degrees circle'.c }
  | Ellipse ellipse' ->
      Ellipse { ellipse' with c = rotate_point degrees ellipse'.c }
  | Line _line -> failwith "Not Implemented"
  | Polygon polygon' ->
      Polygon
        {
          polygon' with
          vertices = List.map (rotate_point degrees) polygon'.vertices;
        }
  | Complex shapes -> Complex (List.map (rotate degrees) shapes)

let compose f g x = g (f x)
let range n = List.init n Fun.id

let repeat n op shape =
  let match_list l =
    match l with [] -> [ op shape ] | last :: _ -> op last :: l
  in
  let shapes = List.fold_right (fun _ acc -> match_list acc) (range n) [] in
  complex shapes

(** Takes a function and a shape and returns a new shape with the 
    function applied to the original's color *)
let rec map_color f = function
  | Circle circle' -> Circle { circle' with color = f circle'.color }
  | Ellipse ellipse' -> Ellipse { ellipse' with color = f ellipse'.color }
  | Line line' -> Line { line' with color = f line'.color }
  | Polygon polygon' -> Polygon { polygon' with color = f polygon'.color }
  | Complex complex' -> Complex (List.map (map_color f) complex')
