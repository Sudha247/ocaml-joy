open Js_of_ocaml

type context = {
  shapes: Shape.shape list ref;
  size: int * int;
  axes: bool;
  elt: Dom_html.divElement Js.t;
}

let create ~size ~axes ~eltId =
  let elt =
    Js.Opt.get
      (Js.Opt.bind
        (Dom_html.document##getElementById (Js.string eltId))
        Dom_html.CoerceTo.div)
      (fun _ -> failwith "Could not find element with id")
  in
  {
    shapes = ref [];
    size;
    axes;
    elt;
  }

let string_of_color (r, g, b, a) =
  Printf.sprintf "rgba(%d, %d, %d, %f)" r g b a

let svg_coords_of_point (w, h) ({ x; y }: float Shape.point) =
  ( x +. (float_of_int w) /. 2.0, (float_of_int h) /. 2.0 -. y )

(* Helper function to convert color to SVG attribute *)
let svg_color_attribute name color =
  Printf.sprintf "%s=\"%s\"" name (string_of_color color)

(* Helper function to convert point to a string *)
let string_of_svg_coords (x, y) =
  Printf.sprintf "%f,%f" x y

(* Function to render a circle *)
let render_circle ctx ({ c; radius; stroke; fill }: Shape.circle) =
  let cx, cy = svg_coords_of_point ctx.size c in
  Printf.sprintf "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" %s %s />"
    cx cy radius
    (svg_color_attribute "stroke" stroke)
    (svg_color_attribute "fill" fill)

(* Function to render an ellipse *)
let render_ellipse ctx ({ c; rx; ry; rotation; stroke; fill }: Shape.ellipse) =
  let cx, cy = svg_coords_of_point ctx.size c in
  Printf.sprintf "<ellipse cx=\"%f\" cy=\"%f\" rx=\"%f\" ry=\"%f\" transform=\"rotate(%d)\" %s %s />"
    cx cy rx ry rotation
    (svg_color_attribute "stroke" stroke)
    (svg_color_attribute "fill" fill)

(* Function to render a polygon *)
let render_polygon ctx ({ vertices; stroke; fill }: Shape.polygon) =
  let string_of_point ({ x; y }: float Shape.point) =
    string_of_svg_coords (svg_coords_of_point ctx.size { x; y })
  in
  let points = String.concat " " (List.map string_of_point vertices) in
  Printf.sprintf "<polygon points=\"%s\" %s %s />"
    points
    (svg_color_attribute "stroke" stroke)
    (svg_color_attribute "fill" fill)

(* Function to render a line *)
let render_line ctx ({ a; b; stroke }: Shape.line) =
  let ax, ay = svg_coords_of_point ctx.size a in
  let bx, by = svg_coords_of_point ctx.size b in
  Printf.sprintf "<line x1=\"%f\" y1=\"%f\" x2=\"%f\" y2=\"%f\" %s />"
    ax ay bx by
    (svg_color_attribute "stroke" stroke)

(* Recursive function to render shapes *)
let rec render_shape ctx s =
  match s with
  | Shape.Circle c -> render_circle ctx c
  | Shape.Ellipse e -> render_ellipse ctx e
  | Shape.Line l -> render_line ctx l
  | Shape.Polygon p -> render_polygon ctx p
  | Shape.Complex shapes -> String.concat "" (List.map (render_shape ctx) shapes)

let make_svg ctx =
  let shapes = !(ctx.shapes) in
  let svg = String.concat "" (List.map (render_shape ctx) shapes) in
  let (width, height) = ctx.size in
  let svg = Printf.sprintf "<svg width=\"%d\" height=\"%d\" xmlns=\"http://www.w3.org/2000/svg\">%s</svg>" width height svg in
  svg

let write ctx =
  let svg = make_svg ctx in
  let elt = ctx.elt in
  elt##.innerHTML := Js.string svg

let show ctx shapes =
  ctx.shapes := !(ctx.shapes) @ shapes;
  write ctx

let clear ctx =
  ctx.shapes := [];
  write ctx

