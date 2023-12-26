module Cairo = Cairo

let tau = 2. *. Float.pi

type point = { x : float; y : float }
type circle = { c : point; radius : float }

type ellipse = {
  start : point;
  curve_one : float * float * float * float * float * float;
  curve_two : float * float * float * float * float * float;
}

type line = { a : point; b : point }
type polygon = point list

type shape =
  | Circle of circle
  | Ellipse of ellipse
  | Line of line
  | Polygon of polygon
  | Complex of shape list

(* point + scalar arithmetic *)
let ( -! ) { x = x1; y = y1 } scalar = { x = x1 -. scalar; y = y1 -. scalar }

(* Global rendering context singleton definition and instantiation *)
type context = {
  ctx : Cairo.context;
  surface : Cairo.Surface.t;
  size : point;
  filename : string;
}

let context = ref None

exception Context of string

(* Not working, could use help fixing *)
let () =
  Printexc.register_printer (fun e ->
      match e with Context err -> Some ("Context: " ^ err) | _ -> None)

let fail () = raise (Context "not initialized")

(* Context initialization, render, and update fns *)
(* Currently, function signature does not match the canvas
   backend function signature, which is a problem.

   Having the end-user call 'set_filename', 'set_size', and
   'set_line_width' isn't particularly satisfying either though *)
let init_context ?line_width (x, y) filename =
  (* Fail if context has already been instantiated *)
  if Option.is_some !context then
    raise (Context "Cannot initialize context twice");

  let surface =
    Cairo.Image.create Cairo.Image.ARGB32 ~w:(int_of_float x)
      ~h:(int_of_float y)
  in
  let ctx = Cairo.create surface in
  Cairo.scale ctx x y;
  Cairo.set_line_width ctx (match line_width with Some n -> n | None -> 0.002);
  context := Some { ctx; surface; size = { x; y }; filename }

(* Renders context to PNG *)
let write ctx = Cairo.PNG.write ctx.surface ctx.filename

(* gets surface size in range 0..pixels *)
let resolution () = match !context with Some ctx -> ctx.size | None -> fail ()

(* sets global color *)
let set_color color =
  match !context with
  | Some ctx ->
      let r, g, b, a = color in
      Cairo.set_source_rgba ctx.ctx r g b a
  | None -> fail ()

(* sets background color *)
let background color =
  match !context with
  | Some ctx ->
      let r, g, b = color in
      Cairo.set_source_rgb ctx.ctx r g b;
      Cairo.paint ctx.ctx
  | None -> fail ()

(* Scales points from 0-image size to 0-1 on both axes *)
let scale_point size point =
  let { x; y } = point in
  let x, y = (x /. size.x, y /. size.y) in
  (x, y)

(* Shape rendering fns *)

(* Circle *)
let circle ?point radius =
  match point with
  | Some c -> Circle { c; radius }
  | None -> Circle { c = { x = 0.; y = 0. }; radius }

let draw_circle ctx ({ c; radius } : circle) =
  let x, y = scale_point ctx.size c in
  let radius = radius /. min ctx.size.x ctx.size.y in
  Cairo.arc ctx.ctx x y ~r:radius ~a1:0. ~a2:tau;
  Cairo.stroke ctx.ctx

(* Rectangle *)
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

let rectangle ?point width height =
  match point with
  | Some c -> make_rectangle c width height
  | None -> make_rectangle { x = 0.; y = 0. } width height

(* Line *)
let line ?point b =
  match point with
  | Some a -> Line { a; b }
  | None -> Line { a = { x = 0.; y = 0. }; b }

let draw_line ctx line =
  let x1, y1 = scale_point ctx.size line.a in
  let x2, y2 = scale_point ctx.size line.b in
  Cairo.move_to ctx.ctx x1 y1;
  Cairo.line_to ctx.ctx x2 y2;
  Cairo.stroke ctx.ctx;
  Cairo.move_to ctx.ctx 0. 0.

(* Ellipse helper fn & rendering fn

   currently just multiplying radii by 2 to offset scaling issue
   feels hacky *)
let ellipse ?point rx ry =
  let c = match point with Some p -> p | None -> { x = 0.; y = 0. } in
  let size = resolution () in
  let x, y = scale_point size c in
  let half_height = ry /. size.y in
  let width_two_thirds = rx /. size.x *. (2. /. 3.) *. 2. in
  Ellipse
    {
      start = { x; y = y -. half_height };
      curve_one =
        ( x +. width_two_thirds,
          y -. half_height,
          x +. width_two_thirds,
          y +. half_height,
          x,
          y +. half_height );
      curve_two =
        ( x -. width_two_thirds,
          y +. half_height,
          x -. width_two_thirds,
          y -. half_height,
          x,
          y -. half_height );
    }

let draw_ellipse (ctx : context) { start; curve_one; curve_two } =
  Cairo.save ctx.ctx;
  Cairo.move_to ctx.ctx start.x start.y;
  let x1, y1, x2, y2, x3, y3 = curve_one in
  Cairo.curve_to ctx.ctx x1 y1 x2 y2 x3 y3;
  let x1, y1, x2, y2, x3, y3 = curve_two in
  Cairo.curve_to ctx.ctx x1 y1 x2 y2 x3 y3;
  Cairo.stroke ctx.ctx;
  Cairo.restore ctx.ctx

(* Polygon helper fns and rendering fn *)
let polygon points = Polygon points

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

(* Complex *)
let complex shapes = Complex shapes

(* Root render fn *)
let rec render_shape ctx shape =
  (match shape with
  | Circle circle -> draw_circle ctx circle
  | Ellipse ellipse -> draw_ellipse ctx ellipse
  | Line line -> draw_line ctx line
  | Polygon polygon -> draw_polygon ctx polygon
  | Complex complex -> List.iter (render_shape ctx) complex);
  write ctx

(* Validates context before rendering *)
let render shape =
  match !context with Some ctx -> render_shape ctx shape | None -> fail ()

(* 'sketch' functions - this is a prototype of what the user would be writing *)
let draw () =
  let { x = w; y = h } = resolution () in
  let c = { x = w /. 2.; y = h /. 2. } in
  let circle = circle ~point:c 100. in
  let rect = rectangle ~point:c (w /. 4.) (h /. 4.) in
  let ellip = ellipse ~point:c 100. 75. in
  let polygon =
    polygon
      (List.map
         (fun { x; y } -> { x = x +. 10.; y = y +. 10. })
         [ c; { x = c.x; y = c.y +. 100. }; { x = c.x +. 100.; y = c.y } ])
  in
  let axes =
    complex
      [
        line ~point:{ x = w /. 2.; y = 0. } { x = w /. 2.; y = h };
        line ~point:{ x = 0.; y = h /. 2. } { x = w; y = h /. 2. };
      ]
  in
  let complex = Complex [ circle; rect; ellip; polygon; axes ] in
  render complex

let setup () =
  let x, y = (800., 800.) in
  let filename = "cairo.png" in
  init_context (x, y) filename;
  background (1., 1., 1.);
  set_color (0., 0., 0., 1.);
  draw ()

let _ = setup ()
