module Svg : Modules.Backend = struct
  (* Global rendering context singleton definition and instantiation *)
  type context = {
    ctx : Cairo.context;
    surface : Cairo.Surface.t;
    size : int * int;
    axes : bool;
  }

  (* Renders context to PNG *)
  let write ctx filename = Cairo.PNG.write ctx.surface filename
  let context = ref None

  exception Context of string

  (* Not working, could use help fixing *)
  let () =
    Printexc.register_printer (fun e ->
        match e with Context err -> Some ("Context: " ^ err) | _ -> None)

  let fail () = raise (Context "not initialized")

  let init_context line_width (w, h) axes =
    (* Fail if context has already been instantiated *)
    if Option.is_some !context then
      raise (Context "Cannot initialize context twice");

    let surface = Cairo.Image.create Cairo.Image.ARGB32 ~w ~h in
    let ctx = Cairo.create surface in
    Cairo.scale ctx (float_of_int w) (float_of_int h);
    Cairo.set_line_width ctx line_width;
    context := Some { ctx; surface; size = (w, h); axes }

  let axes ctx = ctx.axes

  let resolution () =
    match !context with Some ctx -> ctx.size | None -> fail ()

  let tmap3 f (a, b, c) = (f a, f b, f c)
  let tmap4 f (a, b, c, d) = (f a, f b, f c, f d)
  let ( >> ) f g x = g (f x)
  let scale_color_channel x = x /. 256.

  let set_color color =
    match !context with
    | Some ctx ->
        let r, g, b = tmap3 (float_of_int >> scale_color_channel) color in
        Cairo.set_source_rgba ctx.ctx r g b 1.
    | None -> fail ()

  (* sets background color *)
  let background color =
    match !context with
    | Some ctx ->
        let r, g, b, a = tmap4 (float_of_int >> scale_color_channel) color in
        Cairo.set_source_rgba ctx.ctx r g b a;
        Cairo.paint ctx.ctx
    | None -> fail ()

  (** Sets the width of lines for both stroke of shapes and line primitives. 
    Can be any positive integer, with larger numbers producing thicker lines. 
    default is 2 *)
  let set_line_width line_width =
    match !context with
    | Some ctx -> Cairo.set_line_width ctx.ctx (float_of_int line_width /. 1000.)
    | None -> fail ()

  let save () =
    match !context with Some ctx -> Cairo.save ctx.ctx | None -> fail ()

  let restore () =
    match !context with Some ctx -> Cairo.restore ctx.ctx | None -> fail ()

  open Shape

  let tmap f (x, y) = (f x, f y)

  let denormalize point =
    let x, y = resolution () |> tmap float_of_int in
    let canvas_mid = { x; y } /! 2. in
    ((point.x +. canvas_mid.x) /. x, (point.y +. canvas_mid.y) /. y)

  let euclid_norm (x, y) = sqrt (Float.pow x 2. +. Float.pow y 2.) /. 2.

  let draw_circle ctx ({ c; radius } : circle) =
    let size = tmap float_of_int ctx.size in
    let x, y = denormalize c in
    let radius = radius /. euclid_norm size in
    Cairo.arc ctx.ctx x y ~r:radius ~a1:0. ~a2:(Float.pi *. 2.);
    Cairo.stroke ctx.ctx

  let create_control_points { c; rx; ry } =
    let size = resolution () |> tmap float_of_int in
    let x, y = denormalize c in
    let half_height = ry /. snd size in
    let width_two_thirds = rx /. fst size *. (2. /. 3.) *. 2. in
    ( { x; y = y -. half_height },
      ( x +. width_two_thirds,
        y -. half_height,
        x +. width_two_thirds,
        y +. half_height,
        x,
        y +. half_height ),
      ( x -. width_two_thirds,
        y +. half_height,
        x -. width_two_thirds,
        y -. half_height,
        x,
        y -. half_height ) )

  let draw_ellipse ctx ellipse =
    let start, curve_one, curve_two = create_control_points ellipse in
    Cairo.save ctx.ctx;
    Cairo.move_to ctx.ctx start.x start.y;
    let x1, y1, x2, y2, x3, y3 = curve_one in
    Cairo.curve_to ctx.ctx x1 y1 x2 y2 x3 y3;
    let x1, y1, x2, y2, x3, y3 = curve_two in
    Cairo.curve_to ctx.ctx x1 y1 x2 y2 x3 y3;
    Cairo.stroke ctx.ctx;
    Cairo.restore ctx.ctx

  let draw_line ctx line =
    save ();
    let x1, y1 = denormalize line.a in
    let x2, y2 = denormalize line.b in
    Cairo.move_to ctx.ctx x1 y1;
    Cairo.line_to ctx.ctx x2 y2;
    Cairo.stroke ctx.ctx;
    restore ()

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

  let draw_polygon ctx polygon =
    let points = partition 2 ~step:1 (polygon @ [ List.hd polygon ]) in
    List.iter
      (fun pair ->
        let pair = List.map denormalize pair in
        let (x1, y1), (x2, y2) = (List.nth pair 0, List.nth pair 1) in
        Cairo.move_to ctx.ctx x1 y1;
        Cairo.line_to ctx.ctx x2 y2)
      points;
    Cairo.move_to ctx.ctx 0. 0.;
    Cairo.stroke ctx.ctx

  let show shapes =
    let rec render ctx = function
      | Circle circle -> draw_circle ctx circle
      | Ellipse ellipse -> draw_ellipse ctx ellipse
      | Line line -> draw_line ctx line
      | Polygon polygon -> draw_polygon ctx polygon
      | Complex complex -> List.iter (render ctx) complex
    in
    match !context with
    | Some ctx -> List.iter (render ctx) shapes
    | None -> fail ()

  let render_axes () =
    save ();
    let x, y = resolution () |> tmap float_of_int in
    let half_x, half_y = (x /. 2., y /. 2.) in
    let x_axis = line ~a:{ x = 0.; y = -.half_y } { x = 0.; y = half_y } in
    let y_axis = line ~a:{ x = -.half_x; y = 0. } { x = half_x; y = 0. } in
    set_color (0, 0, 0);
    show [ x_axis; y_axis ];
    restore ()
end
