module Cairo = Cairo

let tau = 2. *. Float.pi

type point = { x : float; y : float }
type circle = { c : point; radius : float }
type rectangle = { c : point; width : float; height : float }
type line = { a : point; b : point }
type polygon = point list

type shape =
  | Circle of circle
  | Rectangle of rectangle
  | Line of line
  | Polygon of polygon
  | Complex of shape list

(* Point arithmetic operators

   let ( +~ ) {x = x1; y = y1} {x = x2; y = y2} = {x = x1 +. x2; y = y1 +. y2}
   let ( *~ ) {x = x1; y = y1} {x = x2; y = y2} = {x = x1 *. x2; y = y1 *. y2}
   let ( /~ ) {x = x1; y = y1} {x = x2; y = y2} = {x = x1 /. x2; y = y1 /. y2}
   let ( -~ ) {x = x1; y = y1} {x = x2; y = y2} = {x = x1 -. x2; y = y1 -. y2}

   let ( +! ) {x = x1; y = y1} scalar = {x = x1 +. scalar; y = y1 +. scalar}
   let ( *! ) {x = x1; y = y1} scalar = {x = x1 *. scalar; y = y1 *. scalar}
   let ( /! ) {x = x1; y = y1} scalar = {x = x1 /. scalar; y = y1 /. scalar} *)
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
    Cairo.Image.create Cairo.Image.ARGB32 ~w: (int_of_float w)
      ~h: (int_of_float h)
  in
  let ctx = Cairo.create surface in
  Cairo.scale ctx w h;
  Cairo.set_line_width ctx (match line_width with | Some n -> n | None -> 0.002);
  context := Some { ctx; surface; size = {x = w; y = h}; filename }

(* Renders context to PNG *)
let write ctx = Cairo.PNG.write ctx.surface ctx.filename

let get_dimensions () =
  match !context with Some ctx -> ctx.size | None -> failwith fail

let set_color color =
  match !context with
  | Some ctx ->
      let r, g, b = color in
      Cairo.set_source_rgb ctx.ctx r g b
  | None -> failwith fail

let background color =
  match !context with
  | Some ctx ->
      let r, g, b = color in
      Cairo.set_source_rgb ctx.ctx r g b;
      Cairo.paint ctx.ctx
  | None -> failwith fail

let scale_point size point =
  let { x; y } = point in
  let x, y = (x /. size.x, y /. size.y) in
  List.iter (fun n -> print_float n |> print_newline) [ x; y ];
  (x, y)

let draw_circle ctx (circle : circle) =
  let x, y = scale_point ctx.size circle.c in
  let radius = circle.radius /. min ctx.size.x ctx.size.y in
  Cairo.arc ctx.ctx x y ~r:radius ~a1:0. ~a2:tau;
  Cairo.stroke ctx.ctx

let draw_rect ctx (rect : rectangle) =
  let x, y =
    scale_point ctx.size (rect.c -! ((rect.width +. rect.height) /. 4.))
  in
  let w = rect.width /. ctx.size.x in
  let h = rect.height /. ctx.size.y in
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
  let _ =
    match shape with
    | Circle circle -> draw_circle ctx circle
    | Rectangle rectangle -> draw_rect ctx rectangle
    | Line line -> draw_line ctx line
    | Polygon polygon -> draw_polygon ctx polygon
    | Complex complex -> List.iter (render_shape ctx) complex
  in
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
  let _cartesian_product l l' =
    List.concat (List.map (fun e -> List.map (fun e' -> (e, e')) l') l)
  in
  let polygon =
    Polygon (List.map (fun {x; y} -> {x = x +. 10.; y = y +. 10.})[ c; { x = c.x; y = c.y +. 100. }; { x = c.x +. 100.; y = c.y } ])
  in

  let axes =
    Complex
      [
        Line { a = { x = w /. 2.; y = 0. }; b = { x = w /. 2.; y = h } };
        Line { a = { x = 0.; y = h /. 2. }; b = { x = w; y = h /. 2. } };
      ]
  in
  let complex = Complex [ circle; rect; polygon; axes ] in
  render complex

let init ?size ?filename () =
  let size = match size with Some s -> s | None -> { x = 800.; y = 800. } in
  let { x; y } = size in
  let filename = match filename with Some s -> s | None -> "cairo.png" in
  init_context (x, y) filename;
  background (1., 1., 1.);
  set_color (0., 0., 0.);
  draw ()

let () = init ()
