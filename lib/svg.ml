module S : Modules.Impl = struct
  (* Global rendering context singleton definition and instantiation *)
  type context = {
    ctx : Cairo.context;
    surface : Cairo.Surface.t;
    size : int * int;
    axes : bool;
  }

  (* Renders context to PNG *)
  let write ctx filename =
    Cairo.PNG.write ctx.surface filename;
    Cairo.Surface.finish ctx.surface

  let context = ref None

  exception Context of string

  (* Not working, could use help fixing *)
  let () =
    Printexc.register_printer (fun e ->
        match e with Context err -> Some ("Context: " ^ err) | _ -> None)

  let fail () = raise (Context "not initialized")
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
        Cairo.set_source_rgb ctx.ctx r g b
    | None -> fail ()

  (* sets background color *)
  let background { ctx; _ } color =
    let r, g, b, alpha = tmap4 (float_of_int >> scale_color_channel) color in
    Cairo.set_source_rgb ctx r g b;
    Cairo.paint ctx ~alpha;
    Cairo.fill ctx

  (** Sets the width of lines for both stroke of shapes and line primitives. 
    Can be any positive integer, with larger numbers producing thicker lines. 
    default is 2 *)
  let set_line_width line_width =
    match !context with
    | Some ctx -> Cairo.set_line_width ctx.ctx (float_of_int line_width)
    | None -> fail ()

  let save () =
    match !context with Some ctx -> Cairo.save ctx.ctx | None -> fail ()

  let restore () =
    match !context with Some ctx -> Cairo.restore ctx.ctx | None -> fail ()

  let init_context background_color line_width (w, h) axes =
    (* Fail if context has already been instantiated *)
    if Option.is_some !context then
      raise (Context "Cannot initialize context twice");

    let surface = Cairo.Image.create Cairo.Image.ARGB32 ~w ~h in
    let ctx = Cairo.create surface in
    Cairo.set_line_width ctx (float_of_int line_width);
    Cairo.translate ctx (w / 2 |> float_of_int) (h / 2 |> float_of_int);
    let temp = { ctx; surface; size = (w, h); axes } in
    context := Some temp;
    background temp background_color

  open Shape

  let tmap f (x, y) = (f x, f y)

  let draw_circle ctx ({ c; radius; stroke; fill } : circle) =
    let stroke_circle stroke =
      set_color stroke;
      Cairo.stroke_preserve ctx.ctx
    in
    let fill_circle fill =
      set_color fill;
      Cairo.fill_preserve ctx.ctx
    in
    Cairo.arc ctx.ctx c.x c.y ~r:radius ~a1:0. ~a2:(Float.pi *. 2.);
    Option.iter stroke_circle stroke;
    Option.iter fill_circle fill;
    Cairo.Path.clear ctx.ctx

  let create_control_points ({ x; y }, rx, ry) =
    let half_height = ry /. 2. in
    let width_two_thirds = rx *. (2. /. 3.) *. 2. in
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

  let draw_ellipse ctx { c; rx; ry; stroke; fill } =
    let stroke_ellipse stroke =
      set_color stroke;
      Cairo.stroke_preserve ctx.ctx
    in
    let fill_ellipse fill =
      set_color fill;
      Cairo.fill_preserve ctx.ctx
    in
    let start, curve_one, curve_two = create_control_points (c, rx, ry) in
    Cairo.move_to ctx.ctx start.x start.y;
    let x1, y1, x2, y2, x3, y3 = curve_one in
    Cairo.curve_to ctx.ctx x1 y1 x2 y2 x3 y3;
    let x1, y1, x2, y2, x3, y3 = curve_two in
    Cairo.curve_to ctx.ctx x1 y1 x2 y2 x3 y3;
    Option.iter stroke_ellipse stroke;
    Option.iter fill_ellipse fill;
    Cairo.Path.clear ctx.ctx

  let draw_line ctx { a; b; stroke } =
    set_color stroke;
    let { x; y } = a in
    Cairo.move_to ctx.ctx x y;
    let { x; y } = b in
    Cairo.line_to ctx.ctx x y;
    Cairo.stroke ctx.ctx

  let rec take n lst =
    match (n, lst) with
    | 0, _ -> ([], lst)
    | _, [] -> ([], [])
    | n, x :: xs ->
        let taken, rest = take (n - 1) xs in
        (x :: taken, rest)

  let rec partition n ?(step = 0) = function
    | [] -> []
    | lst ->
        let taken, _ = take n lst in
        if List.length taken = n then taken :: partition n ~step (List.tl lst)
        else []

  let draw_polygon ctx { vertices = points; stroke; fill } =
    let stroke_rect stroke =
      set_color stroke;
      Cairo.stroke_preserve ctx.ctx
    in
    let fill_rect fill =
      set_color fill;
      Cairo.fill_preserve ctx.ctx
    in
    let points = partition 2 ~step:1 (points @ [ List.hd points ]) in
    List.iter
      (fun pair ->
        let { x = x1; y = y1 }, { x = x2; y = y2 } =
          (List.nth pair 0, List.nth pair 1)
        in
        Cairo.move_to ctx.ctx x1 y1;
        Cairo.line_to ctx.ctx x2 y2)
      points;
    Option.iter stroke_rect stroke;
    Option.iter fill_rect fill;
    Cairo.Path.clear ctx.ctx

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
    let x, y = resolution () |> tmap float_of_int in
    let half_x, half_y = (x /. 2., y /. 2.) in
    let x_axis = line ~a:{ x = 0.; y = -.half_y } { x = 0.; y = half_y } in
    let y_axis = line ~a:{ x = -.half_x; y = 0. } { x = half_x; y = 0. } in
    show [ x_axis; y_axis ]
end

module Backend = Modules.Make (S)
