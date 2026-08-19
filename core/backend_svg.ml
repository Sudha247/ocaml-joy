let string_of_color (r, g, b, a) =
  Printf.sprintf "rgba(%d, %d, %d, %f)" r g b a

let svg_coords_of_point (w, h) ({ x; y } : Shape.point) =
  (x +. (float_of_int w /. 2.0), (float_of_int h /. 2.0) -. y)

let svg_color_attribute name color =
  Printf.sprintf "%s=\"%s\"" name (string_of_color color)

let string_of_svg_coords (x, y) = Printf.sprintf "%f,%f" x y

let render_circle size ({ c; radius; stroke; fill } : Shape.circle) =
  let cx, cy = svg_coords_of_point size c in
  Printf.sprintf "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" %s %s />" cx cy radius
    (svg_color_attribute "stroke" stroke)
    (svg_color_attribute "fill" fill)

let render_ellipse size ({ c; rx; ry; rotation; stroke; fill } : Shape.ellipse) =
  let cx, cy = svg_coords_of_point size c in
  Printf.sprintf
    "<ellipse cx=\"%f\" cy=\"%f\" rx=\"%f\" ry=\"%f\" transform=\"rotate(%d %f %f)\" %s %s />"
    cx cy rx ry rotation cx cy
    (svg_color_attribute "stroke" stroke)
    (svg_color_attribute "fill" fill)

let render_polygon size ({ vertices; stroke; fill } : Shape.polygon) =
  let string_of_point ({ x; y } : Shape.point) =
    string_of_svg_coords (svg_coords_of_point size { x; y })
  in
  let points = String.concat " " (List.map string_of_point vertices) in
  Printf.sprintf "<polygon points=\"%s\" %s %s />" points
    (svg_color_attribute "stroke" stroke)
    (svg_color_attribute "fill" fill)

let render_line size ({ a; b; stroke } : Shape.line) =
  let ax, ay = svg_coords_of_point size a in
  let bx, by = svg_coords_of_point size b in
  Printf.sprintf "<line x1=\"%f\" y1=\"%f\" x2=\"%f\" y2=\"%f\" %s />" ax ay
    bx by
    (svg_color_attribute "stroke" stroke)

let rec render_shape size s =
  match s with
  | Shape.Circle c -> render_circle size c
  | Shape.Ellipse e -> render_ellipse size e
  | Shape.Line l -> render_line size l
  | Shape.Polygon p -> render_polygon size p
  | Shape.Complex shapes ->
      String.concat "" (List.map (render_shape size) shapes)

let render ~size shapes =
  let width, height = size in
  let body = String.concat "" (List.map (render_shape size) shapes) in
  Printf.sprintf
    "<svg width=\"%d\" height=\"%d\" xmlns=\"http://www.w3.org/2000/svg\">%s</svg>"
    width height body
