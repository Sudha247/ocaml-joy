type point = { x : float; y : float }
type line = { a : point; b : point }
type circle = { c : point; radius : float }
type ellipse = { c : point; rx : float; ry : float }
type polygon = point list

type shape =
  | Circle of circle
  | Ellipse of ellipse
  | Line of line
  | Polygon of polygon
  | Complex of shape list

type shapes = shape list

(* point - point arithmetic *)
let ( /~ ) p1 p2 = { x = p1.x /. p2.x; y = p1.x /. p2.x }

(* point + scalar arithmetic *)
let ( -! ) { x = x1; y = y1 } scalar = { x = x1 -. scalar; y = y1 -. scalar }
let default_line_width = 0.002
let default_filename = "cairo.png"
let default_size = (800., 800.)

(* Global rendering context singleton definition and instantiation *)
type joy_context = {
  ctx : Cairo.context;
  surface : Cairo.Surface.t;
  size : point;
  filename : string;
}

(* Renders context to PNG *)
let write ctx = Cairo.PNG.write ctx.surface ctx.filename
let context = ref None

exception Context of string

(* Not working, could use help fixing *)
let () =
  Printexc.register_printer (fun e ->
      match e with Context err -> Some ("Context: " ^ err) | _ -> None)

let fail () = raise (Context "not initialized")

let get_window_size () =
  match !context with Some ctx -> ctx.size | None -> fail ()

let bi_to_uni { x; y } =
  let size = get_window_size () in
  let nx = (x *. 0.5) +. (size.x *. 0.5) in
  let ny = (y *. 0.5) +. (size.y *. 0.5) in
  (nx, ny)

(* Scales points from 0-image size to 0-1 on both axes *)
let scale_point size point =
  let { x; y } = point in
  let x, y = (x /. size.x, y /. size.y) in
  (x, y)

let denormalize point =
  let size = get_window_size () in
  let canvas_mid = size /~ { x = 2.; y = 2. } in
  { x = point.x +. canvas_mid.x; y = point.y +. canvas_mid.y }

let point x y = { x; y }

let circle ?point r =
  match point with
  | Some point -> Circle { c = point; radius = r }
  | _ -> Circle { c = { x = 0.; y = 0. }; radius = r }

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

let rectangle ?point length width =
  make_rectangle
    (match point with Some c -> c | None -> { x = 0.; y = 0. })
    length width

let ellipse ?point rx ry =
  match point with
  | Some c -> Ellipse { c; rx; ry }
  | None -> Ellipse { c = { x = 0.; y = 0. }; rx; ry }

let line ?point point_b =
  match point with
  | Some p -> Line { a = p; b = point_b }
  | _ -> Line { a = { x = 0.; y = 0. }; b = point_b }

let polygon lst_points = Polygon lst_points

let complex shapes =
  match shapes with _ :: _ -> Complex shapes | [] -> Complex []

let init_context line_width (x, y) filename =
  (* Fail if context has already been instantiated *)
  if Option.is_some !context then
    raise (Context "Cannot initialize context twice");

  let surface =
    Cairo.Image.create Cairo.Image.ARGB32 ~w:(int_of_float x)
      ~h:(int_of_float y)
  in
  let ctx = Cairo.create surface in
  Cairo.scale ctx x y;
  Cairo.set_line_width ctx line_width;
  context := Some { ctx; surface; size = { x; y }; filename }

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
      let r, g, b, a = color in
      Cairo.set_source_rgba ctx.ctx r g b a;
      Cairo.paint ctx.ctx
  | None -> fail ()

let init ?line_width ?size ?filename () =
  let lw = Option.value line_width ~default:default_line_width in
  let sz = Option.value size ~default:default_size in
  let file = Option.value filename ~default:default_filename in
  init_context lw sz file
