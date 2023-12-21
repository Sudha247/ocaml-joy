(* JS deps *)
module Html = Js_of_ocaml.Dom_html
module Dom = Js_of_ocaml.Dom
module Js = Js_of_ocaml.Js
module G = Graphics_js

type point = { x : float; y : float }

let ( -! ) { x; y } scalar = { x = x -. scalar; y = y -. scalar }
let ( +! ) { x; y } scalar = { x = x +. scalar; y = y +. scalar }
let ( *! ) { x; y } scalar = { x = x *. scalar; y = y *. scalar }

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

exception Context of string

(* Not working, could use help fixing *)
let () =
  Printexc.register_printer (fun e ->
      match e with Context err -> Some ("Context: " ^ err) | _ -> None)

let fail () = raise (Context "not initialized")

let init_context canvas =
  if Option.is_some !context then
    raise (Context "cannot iniitialize context twice")
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

(* transformations  *)
let deg_to_rad degrees = degrees *. (Stdlib.Float.pi /. 180.)
let bi_to_uni point = (point *! 0.5) +! 0.5

let rotate_point degrees { x; y } =
  let radians = deg_to_rad degrees in
  let dx = (x *. cos radians) -. (y *. sin radians) in
  let dy = (x *. sin radians) +. (y *. cos radians) in
  bi_to_uni { x = dx; y = dy }

let rotate_ellipse deg { start; curve_one; curve_two } =
  let destructure_list = function
    | [ (x1, y1); (x2, y2); (x3, y3) ] -> Ok (x1, y1, x2, y2, x3, y3)
    | _ -> Error (Context "Error  in ellipse rotation")
  in
  let map_curve curve =
    let x1, y1, x2, y2, x3, y3 = curve in
    let points =
      List.map
        (fun point ->
          let { x; y } = rotate_point deg point in
          (x, y))
        [ { x = x1; y = y1 }; { x = x2; y = y2 }; { x = x3; y = y3 } ]
    in
    Result.get_ok (destructure_list points)
  in
  let start = rotate_point deg start in
  let curve_one = map_curve curve_one in
  let curve_two = map_curve curve_two in
  Ellipse { start; curve_one; curve_two }

let rec rotate degrees shape =
  match shape with
  | Circle circle ->
      Circle { c = rotate_point degrees circle.c; radius = circle.radius }
  | Ellipse ellipse -> rotate_ellipse degrees ellipse
  | Line _line -> failwith "Not Implemented"
  | Polygon polygon' -> polygon (List.map (rotate_point degrees) polygon')
  | Complex shapes -> Complex (List.map (rotate degrees) shapes)

let repeat n op shape =
  let identity n = n in
  let match_list l =
    match l with [] -> [ shape ] | last :: _ -> op last :: l
  in
  let shapes =
    List.fold_right (fun _ acc -> match_list acc) (List.init n identity) []
  in
  complex shapes

(* Render fns *)

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
  let _complex = complex [ rect; ellip; circle; polygon; axes ] in
  let ellipses = repeat 4 (rotate (360. /. 32.)) (ellipse ~point:c 100. 75.) in
  render ellipses

let onload _ =
  let canvas = create_canvas () in
  init_context canvas;
  maximize_canvas ();
  draw ();
  Js._true

let _ = window##.onload := Html.handler onload
