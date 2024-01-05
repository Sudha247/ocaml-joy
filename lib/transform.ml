open Shape

let rec translate dx dy shape =
  match shape with
  | Circle circle ->
      Circle { circle with c = { x = circle.c.x +. dx; y = circle.c.y +. dy } }
  | Ellipse ellipse ->
      Ellipse
        { ellipse with c = { x = ellipse.c.x +. dx; y = ellipse.c.y +. dy } }
  | Line line ->
      Line
        {
          a = { x = line.a.x +. dx; y = line.a.y +. dy };
          b = { x = line.b.x +. dx; y = line.b.y +. dy };
        }
  | Polygon polygon' ->
      polygon (List.map (fun { x; y } -> { x = x +. dx; y = y +. dy }) polygon')
  | Complex shapes -> Complex (List.map (translate dx dy) shapes)

let rec scale factor s =
  let scale_length fact len = len *. sqrt fact in
  let scale_point fact pt = pt *! sqrt fact in
  match s with
  | Circle circle' ->
      Circle { c = scale_point factor circle'.c; radius = scale_length factor circle'.radius  }
  | Ellipse ellipse' ->
      Ellipse
        {
          c = scale_point factor ellipse'.c; 
          rx = scale_length factor ellipse'.rx;
          ry = scale_length factor ellipse'.ry;
        }
  | Line _line' -> failwith "Not Implemented"
  | Polygon polygon' ->
      let scale_point factor { x; y } =
        { x = scale_length factor x; y = scale_length factor y }
      in
      polygon (List.map (scale_point factor) polygon')
  | Complex shapes -> Complex (List.map (scale factor) shapes)

let to_radians degrees = float_of_int degrees *. Stdlib.Float.pi /. 180.

let to_polar point =
  let { x; y } = point in
    ( sqrt ((x *. x) +. (y *. y)),
      atan2 y x )

let from_polar polar_point =
  let (r, theta) = polar_point in
  { x = r *. cos theta; y = r *. sin theta }

let rotate_point degrees point =
  let radians = to_radians degrees in
  let (r, theta) = to_polar point in
  from_polar (r, theta +. radians)

let rec rotate degrees shape =
  match shape with
  | Circle circle' -> Circle { circle' with c = rotate_point degrees circle'.c }
  | Ellipse ellipse' -> Ellipse { ellipse' with c = rotate_point degrees ellipse'.c }
  | Line _line -> failwith "Not Implemented"
  | Polygon polygon' -> polygon (List.map (rotate_point degrees) polygon')
  | Complex shapes -> Complex (List.map (rotate degrees) shapes)

let compose f g x = g (f x)