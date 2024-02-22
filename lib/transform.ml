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
              (fun { x; y } ->
                { x = x +. float_of_int dx; y = y +. float_of_int dy })
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
  | Line line' -> Line { line' with b = rotate_point degrees line'.b }
  | Polygon polygon' ->
      Polygon
        {
          polygon' with
          vertices = List.map (rotate_point degrees) polygon'.vertices;
        }
  | Complex shapes -> Complex (List.map (rotate degrees) shapes)

let compose f g x = g (f x)

let repeat n op shape =
  let rec repeat' = function
    | 0, shapes -> shapes
    | n, [] -> repeat' (n - 1, [ shape ])
    | n, (transformed :: _ as shapes) ->
        repeat' (n - 1, op transformed :: shapes)
  in
  Complex (repeat' (n, []))

(** Takes a function and a shape and returns a new shape with the 
    function applied to the original's color *)
let rec map_stroke f = function
  | Circle circle' ->
      Circle { circle' with stroke = Option.map f circle'.stroke }
  | Ellipse ellipse' ->
      Ellipse { ellipse' with stroke = Option.map f ellipse'.stroke }
  | Line line' -> Line { line' with stroke = f line'.stroke }
  | Polygon polygon' ->
      Polygon { polygon' with stroke = Option.map f polygon'.stroke }
  | Complex complex' -> Complex (List.map (map_stroke f) complex')

let rec map_fill f = function
  | Circle circle' -> Circle { circle' with fill = Option.map f circle'.fill }
  | Ellipse ellipse' ->
      Ellipse { ellipse' with fill = Option.map f ellipse'.fill }
  | Polygon polygon' ->
      Polygon { polygon' with fill = Option.map f polygon'.fill }
  | Complex complex' -> Complex (List.map (map_fill f) complex')
  | _ as line' ->
      print_endline "Lines do not have a fill field!";
      line'