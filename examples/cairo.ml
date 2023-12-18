module Cairo = Cairo

let tau = 2. *. Float.pi

type point = { x : float; y : float }
type circle = { c : point; radius : float }
type ellipse = { c : point; rx : float; ry : float }
type rectangle = { c : point; width : float; height : float }
type line = { a : point; b : point }
type polygon = point list

type shape =
  | Circle of circle
  | Ellipse of ellipse
  | Rectangle of rectangle
  | Line of line
  | Polygon of polygon
  | Complex of shape list

(* Point arithmetic operators
    I think thesse would be useful, 
    but also undeerstand not everyone likes the arithmetic operator 
    symbol overload thing

   let ( +~ ) {x = x1; y = y1} {x = x2; y = y2} = {x = x1 +. x2; y = y1 +. y2}
   let ( *~ ) {x = x1; y = y1} {x = x2; y = y2} = {x = x1 *. x2; y = y1 *. y2}

   let ( -~ ) {x = x1; y = y1} {x = x2; y = y2} = {x = x1 -. x2; y = y1 -. y2}

   let ( +! ) {x = x1; y = y1} scalar = {x = x1 +. scalar; y = y1 +. scalar}
   let ( *! ) {x = x1; y = y1} scalar = {x = x1 *. scalar; y = y1 *. scalar}
   let ( /! ) {x = x1; y = y1} scalar = {x = x1 /. scalar; y = y1 /. scalar} *)

(* point + point arithmetic *)
let ( /~ ) { x = x1; y = y1 } { x = x2; y = y2 } =
  { x = x1 /. x2; y = y1 /. y2 }

(* point + scalar arithmetic *)
let ( -! ) { x = x1; y = y1 } scalar = { x = x1 -. scalar; y = y1 -. scalar }

(* Global rendering context singleton definition and instantiation *)
type cairo_context = {
  ctx : Cairo.context;
  surface : Cairo.Surface.t;
  size : point;
  filename : string;
}

let context = ref None

(* Error message *)
let fail = "Context not initialized"

(* Context initialization, render, and update fns *)
let init_context ?line_width (w, h) filename =
  let surface =
    Cairo.Image.create Cairo.Image.ARGB32 ~w:(int_of_float w)
      ~h:(int_of_float h)
  in
  let ctx = Cairo.create surface in
  Cairo.scale ctx w h;
  Cairo.set_line_width ctx (match line_width with Some n -> n | None -> 0.002);
  context := Some { ctx; surface; size = { x = w; y = h }; filename }

(* Renders context to PNG *)
let write ctx = Cairo.PNG.write ctx.surface ctx.filename

let get_dimensions () =
  match !context with Some ctx -> ctx.size | None -> failwith fail

let set_color color =
  match !context with
  | Some ctx ->
      let r, g, b, a = color in
      Cairo.set_source_rgba ctx.ctx r g b a
  | None -> failwith fail

let background color =
  match !context with
  | Some ctx ->
      let r, g, b = color in
      Cairo.set_source_rgb ctx.ctx r g b;
      Cairo.paint ctx.ctx
  | None -> failwith fail

(* Scales points from 0 - image size to 0-1 *)
let scale_point size point =
  let { x; y } = point in
  let x, y = (x /. size.x, y /. size.y) in
  (x, y)

(* Shape rendering fns *)
let draw_circle ctx ({ c; radius } : circle) =
  let x, y = scale_point ctx.size c in
  let radius = radius /. min ctx.size.x ctx.size.y in
  Cairo.arc ctx.ctx x y ~r:radius ~a1:0. ~a2:tau;
  Cairo.stroke ctx.ctx

(* Ellipse helper fn & rendering fn *)

let calculate_control_points (size : point) ({ c; rx; ry } : ellipse) =
  let { x; y } = c in
  let { x = w; y = h } = size in
  let half_height = ry /. 2. in
  let width_two_thirds = rx *. (2. /. 3.) in
  ( { x; y = y -. half_height } /~ size,
    ( (x +. width_two_thirds) /. w,
      (y -. half_height) /. h,
      (x +. width_two_thirds) /. w,
      (y +. half_height) /. h,
      x /. w,
      (y +. half_height) /. h ),
    ( (x -. width_two_thirds) /. w,
      (y +. half_height) /. h,
      (x -. width_two_thirds) /. w,
      (y -. half_height) /. h,
      x /. w,
      (y -. half_height) /. h ) )

let draw_ellipse (ctx : cairo_context) (ellipse : ellipse) =
  let start, curve_one, curve_two = calculate_control_points ctx.size ellipse in
  Cairo.save ctx.ctx;
  Cairo.move_to ctx.ctx start.x start.y;
  let x1, y1, x2, y2, x3, y3 = curve_one in
  Cairo.curve_to ctx.ctx x1 y1 x2 y2 x3 y3;
  let x1, y1, x2, y2, x3, y3 = curve_two in
  Cairo.curve_to ctx.ctx x1 y1 x2 y2 x3 y3;
  Cairo.stroke ctx.ctx;
  Cairo.restore ctx.ctx

let draw_rect ctx ({ c; width; height } : rectangle) =
  let x, y = scale_point ctx.size (c -! ((width +. height) /. 4.)) in
  let w = width /. ctx.size.x in
  let h = height /. ctx.size.y in
  Cairo.rectangle ctx.ctx x y ~w ~h;
  Cairo.stroke ctx.ctx

let draw_line ctx line =
  let x1, y1 = scale_point ctx.size line.a in
  let x2, y2 = scale_point ctx.size line.b in
  Cairo.move_to ctx.ctx x1 y1;
  Cairo.line_to ctx.ctx x2 y2;
  Cairo.stroke ctx.ctx;
  Cairo.move_to ctx.ctx 0. 0.

(* Polygon helper fns and rendering fn *)
let rec take n lst =
  match (n, lst) with
  | 0, _ -> ([], lst)
  | _, [] -> ([], [])
  | n, x :: xs ->
      let taken, rest = take (n - 1) xs in
      (x :: taken, rest)

let rec partition n ?step lst =
  match lst with
  | [] -> []
  | _ ->
      let taken, _ = take n lst in
      if List.length taken = n then
        taken
        ::
        (match step with
        | Some s -> partition n ~step:s (List.tl lst)
        | None -> partition n ~step:0 (List.tl lst))
      else []

let draw_polygon ctx (polygon : polygon) =
  let points = partition 2 ~step:1 (polygon @ [ List.hd polygon ]) in
  List.iter
    (fun pair ->
      let pair = List.map (scale_point ctx.size) pair in
      let (x1, y1), (x2, y2) = (List.nth pair 0, List.nth pair 1) in
      Cairo.move_to ctx.ctx x1 y1;
      Cairo.line_to ctx.ctx x2 y2)
    points;
  Cairo.move_to ctx.ctx 0. 0.;
  Cairo.stroke ctx.ctx

(* Root render fn *)
let rec render_shape ctx shape =
  (match shape with
  | Circle circle -> draw_circle ctx circle
  | Rectangle rectangle -> draw_rect ctx rectangle
  | Ellipse ellipse -> draw_ellipse ctx ellipse
  | Line line -> draw_line ctx line
  | Polygon polygon -> draw_polygon ctx polygon
  | Complex complex -> List.iter (render_shape ctx) complex);
  write ctx

(* Validates context before rendering *)
let render shape =
  match !context with
  | Some ctx -> render_shape ctx shape
  | None -> failwith fail

(* 'sketch' functions - this is a prototype of what the user would be writing *)
let draw () =
  let { x = w; y = h } = get_dimensions () in
  let c = { x = w /. 2.; y = h /. 2. } in
  let circle = Circle { c; radius = 100. } in
  let rect = Rectangle { c; width = w /. 4.; height = h /. 4. } in
  let ellip = Ellipse { c; rx = 75.; ry = 50. } in
  let polygon =
    Polygon
      (List.map
         (fun { x; y } -> { x = x +. 10.; y = y +. 10. })
         [ c; { x = c.x; y = c.y +. 100. }; { x = c.x +. 100.; y = c.y } ])
  in
  let axes =
    Complex
      [
        Line { a = { x = w /. 2.; y = 0. }; b = { x = w /. 2.; y = h } };
        Line { a = { x = 0.; y = h /. 2. }; b = { x = w; y = h /. 2. } };
      ]
  in
  let complex = Complex [ circle; rect; ellip; polygon; axes ] in
  render complex

let init ?size ?filename () =
  let size = match size with Some s -> s | None -> { x = 800.; y = 800. } in
  let { x; y } = size in
  let filename = match filename with Some s -> s | None -> "cairo.png" in
  init_context (x, y) filename;
  background (1., 1., 1.);
  set_color (0., 0., 0., 1.);
  draw ()

let () = init ()
