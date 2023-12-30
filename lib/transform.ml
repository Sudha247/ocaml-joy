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
  match s with
  | Circle circle' ->
      Circle { circle' with radius = scale_length factor circle'.radius }
  | Ellipse ellipse' ->
      Ellipse
        {
          ellipse' with
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

let deg_to_rad degrees = degrees *. (Stdlib.Float.pi /. 180.)

let bi_to_uni { x; y } =
  let cx, cy = Context.resolution () in
  let nx = (x *. 0.5) +. (cx *. 0.5) in
  let ny = (y *. 0.5) +. (cy *. 0.5) in
  (nx, ny)

let rot degrees { x; y } =
  let radians = deg_to_rad (float_of_int degrees) in
  let dx = (x *. cos radians) -. (y *. sin radians) in
  let dy = (x *. sin radians) +. (y *. cos radians) in
  let dx, dy = bi_to_uni { x = dx; y = dy } in
  { x = dx; y = dy }

let rec rotate degrees shape =
  match shape with
  | Circle circle' -> Circle { circle' with c = rot degrees circle'.c }
  | Ellipse ellipse' -> Ellipse { ellipse' with c = rot degrees ellipse'.c }
  | Line _line -> failwith "Not Implemented"
  | Polygon polygon' -> polygon (List.map (rot degrees) polygon')
  | Complex shapes -> Complex (List.map (rotate degrees) shapes)

let compose f g x = g (f x)
