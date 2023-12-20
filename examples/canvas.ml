(* JS deps *)
module Html = Js_of_ocaml.Dom_html
module Dom = Js_of_ocaml.Dom
module Js = Js_of_ocaml.Js
module G = Graphics_js

type point = { x : float; y : float }

let ( -! ) point scalar = { x = point.x -. scalar; y = point.y -. scalar }

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

(* JS type conversion helpers *)
let str = Js.string
let bl = Js.bool

(* aliases for globals *)
let doc = Html.document
let window = Html.window

(* Context *)
type joy_context = {
  context : Html.canvasRenderingContext2D Js.t;
  canvas : Html.canvasElement Js.t;
}

let context : joy_context option ref = ref None
let fail () = window##alert (str "Context not initialized!")

let init_context canvas =
  if Option.is_some !context then
    window##alert (str "Context cannot be initialized twice!")
  else (
    G.open_canvas canvas;
    Dom.appendChild doc##.body canvas;
    let ctx = canvas##getContext Html._2d_ in
    context := Some { context = ctx; canvas })

let get_window_size () =
  let w = float_of_int window##.innerWidth in
  let h = float_of_int window##.innerHeight in
  (w, h)

let maximize_canvas () =
  match !context with
  | Some ctx ->
      let w, h = get_window_size () in
      ctx.canvas##.width := int_of_float w;
      ctx.canvas##.height := int_of_float h
  | None -> fail ()

let create_canvas () =
  let w, h = get_window_size () in
  let canvas = Html.createCanvas doc in
  canvas##.width := int_of_float w;
  canvas##.height := int_of_float h;
  canvas

let color_str (r, g, b) =
  str (Printf.sprintf "rgb(%f, %f, %f)" (r *. 255.) (g *. 255.) (b *. 255.))

(* Sets global color *)
let set_color color =
  match !context with
  | Some { context; canvas = _canvas } ->
      let color_string = color_str color in
      context##.fillStyle := color_string
  | None -> fail ()

(* sets background color *)
let background color =
  match !context with
  | Some { context; canvas = _canvas } ->
      let w, h = get_window_size () in
      let _color_string = color_str color in
      context##.fillStyle := str "white";
      context##fillRect 0. 0. w h
  | None -> fail ()

let circle ?point radius =
  match point with
  | Some c -> Circle { c; radius }
  | None -> Circle { c = { x = 0.; y = 0. }; radius }

let draw_circle ctx { c; radius } =
  let { x; y } = c in
  ctx##beginPath;
  ctx##arc x y radius 0. (2. *. Float.pi) (bl false);
  ctx##stroke

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

let line ?point b =
  match point with
  | Some a -> Line { a; b }
  | None -> Line { a = { x = 0.; y = 0. }; b }

let draw_line ctx { a = { x = x1; y = y1 }; b = { x = x2; y = y2 } } =
  ctx##moveTo x1 y1;
  ctx##lineTo x2 y2;
  ctx##stroke;
  ctx##moveTo 0. 0.

(* Ellipse constructor fn & rendering fn
   currently just multiplying radii by 2 to offset scaling issue
   feels hacky *)
let ellipse ?point rx ry =
  let { x; y } = match point with Some p -> p | None -> { x = 0.; y = 0. } in
  let rx, ry = (rx *. 2., ry *. 2.) in
  let half_height = ry /. 2. in
  let width_two_thirds = rx *. (2. /. 3.) in
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

let draw_ellipse ctx (ellipse : ellipse) =
  let { start; curve_one; curve_two } = ellipse in
  ctx##moveTo start.x start.y;
  let x1, y1, x2, y2, x3, y3 = curve_one in
  ctx##bezierCurveTo x1 y1 x2 y2 x3 y3;
  let x1, y1, x2, y2, x3, y3 = curve_two in
  ctx##bezierCurveTo x1 y1 x2 y2 x3 y3;
  ctx##stroke;
  ctx##moveTo 0. 0.

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
      let { x = x1; y = y1 }, { x = x2; y = y2 } =
        (List.nth pair 0, List.nth pair 1)
      in
      ctx##moveTo x1 y1;
      ctx##lineTo x2 y2)
    points;
  ctx##stroke;
  ctx##moveTo 0. 0.

let complex shapes = Complex shapes

let rec render_shape ctx shape =
  match shape with
  | Circle circle -> draw_circle ctx circle
  | Ellipse ellipse -> draw_ellipse ctx ellipse
  | Line line -> draw_line ctx line
  | Polygon polygon -> draw_polygon ctx polygon
  | Complex complex -> List.iter (render_shape ctx) complex

let render shape =
  match !context with
  | Some ctx -> render_shape ctx.context shape
  | None -> fail ()

let draw () =
  let w, h = get_window_size () in
  let c = { x = w /. 2.; y = h /. 2. } in
  background (1., 1., 1.);
  set_color (0., 0., 0.);
  let circle = circle ~point:c 100. in
  let rect = rectangle ~point:c 100. 100. in
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
  let complex = complex [ rect; ellip; circle; polygon; axes ] in
  render complex

let onload _ =
  let canvas = create_canvas () in
  init_context canvas;
  maximize_canvas ();
  draw ();
  Js._true

let _ = window##.onload := Html.handler onload
