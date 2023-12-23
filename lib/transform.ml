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
  let scale_length len fact = len *. sqrt fact in
  match s with
  | Circle circle' ->
      circle ~point:circle'.c (scale_length circle'.radius factor)
  | Ellipse ellipse' ->
      ellipse ~point:ellipse'.c
        (scale_length ellipse'.rx factor)
        (scale_length ellipse'.ry factor)
  | Line _line' -> failwith "Not Implemented"
  | Polygon _polygon' -> failwith "Scale not implemeted for polygons"
  | Complex shapes -> Complex (List.map (scale factor) shapes)

let deg_to_rad degrees = degrees *. (Stdlib.Float.pi /. 180.)

let rot degrees { x; y } =
  let radians = deg_to_rad (float_of_int degrees) in
  let dx = (x *. cos radians) -. (y *. sin radians) in
  let dy = (x *. sin radians) +. (y *. cos radians) in
  let dx, dy = bi_to_uni { x = dx; y = dy } in
  { x = dx; y = dy }

let rec rotate degrees shape =
  match shape with
  | Circle circle -> Circle { c = rot degrees circle.c; radius = circle.radius }
  | Ellipse ellipse -> Ellipse { ellipse with c = rot degrees ellipse.c }
  | Line _line -> failwith "Not Implemented"
  | Polygon polygon' -> polygon (List.map (rot degrees) polygon')
  | Complex shapes -> Complex (List.map (rotate degrees) shapes)

let compose f g x = g (f x)
