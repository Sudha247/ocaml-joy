module Html = Js_of_ocaml.Dom_html
module Dom = Js_of_ocaml.Dom
module Js = Js_of_ocaml.Js

(* JS type conversion helpers *)
let str = Js.string
let bl = Js.bool

(* aliases for globals *)
let doc = Html.document

(* Needed for 'write'/image save*)
let _window = Html.window

module Canvas : Modules.Backend = struct
  type context = {
    context : Html.canvasRenderingContext2D Js.t;
    size : int * int;
    axes : bool;
  }

  let context : context option ref = ref None

  exception Context of string

  (* Not working, could use help fixing *)
  let () =
    Printexc.register_printer (fun e ->
        match e with Context err -> Some ("Context: " ^ err) | _ -> None)

  let fail () = raise (Context "not initialized")

  let create_canvas size =
    let w, h = size in
    let canvas = Html.createCanvas doc in
    canvas##.width := w;
    canvas##.height := h;
    canvas

  let init_context line_width size axes =
    let canvas = create_canvas size in
    if Option.is_some !context then
      raise (Context "cannot iniitialize context twice")
    else (
      Dom.appendChild doc##.body canvas;
      let ctx = canvas##getContext Html._2d_ in
      ctx##.lineWidth := line_width;
      context := Some { context = ctx; axes; size })

  (* Sets global color *)
  let set_color color =
    match !context with
    | Some { context; _ } ->
        let r, g, b = color in
        let color_string = Printf.sprintf "rgb(%d, %d, %d)" r g b |> str in
        context##.fillStyle := color_string;
        context##.strokeStyle := color_string
    | None -> fail ()

  let begin_path ctx = ctx##beginPath
  let close_path ctx = ctx##closePath

  (* sets background color *)
  let background color =
    let rgba (r, g, b, a) =
      str (Printf.sprintf "rgba(%d, %d, %d, %d)" r g b a)
    in
    match !context with
    | Some ({ context; _ } as ctx) ->
        let col = rgba color in
        let w, h = ctx.size in
        begin_path context;
        context##.fillStyle := col;
        context##fillRect 0. 0. (float_of_int w) (float_of_int h)
    | None -> fail ()

  (* Accessor functions for 'context' *)

  (** Returns 'axes' field of context *)
  let axes ctx = ctx.axes

  (** Returns 'size' field of context *)
  let resolution () =
    match !context with Some ctx -> ctx.size | None -> fail ()

  let set_line_width lw =
    match !context with
    | Some { context = ctx; _ } -> ctx##.lineWidth := float_of_int lw /. 1000.
    | None -> fail ()

  (** TODO: writ this fn, blob API + coercion in JSOO is tough, not sure where 
      all the methods etc I need are *)
  let write _ctx _filename =
    let _save_image _filename _blob =
      (* Create anchor element, bind download fn to anchor,
         and then programmatically click anchor to download png *)
      ()
    in
    (* Coerce canvas to blob, bind callback *)
    ()

  let save () =
    match !context with
    | Some { context = ctx; _ } -> ctx##save
    | None -> fail ()

  let restore () =
    match !context with
    | Some { context = ctx; _ } -> ctx##restore
    | None -> fail ()

  open Shape

  let draw_circle ctx { c; radius } =
    let { x; y } = c in
    begin_path ctx;
    ctx##arc x y radius 0. (2. *. Float.pi) (bl false);
    ctx##stroke;
    close_path ctx

  let draw_line ctx { a = { x = x1; y = y1 }; b = { x = x2; y = y2 } } =
    begin_path ctx;
    ctx##moveTo x1 y1;
    ctx##lineTo x2 y2;
    ctx##stroke;
    ctx##moveTo 0. 0.;
    close_path ctx

  type curve = float * float * float * float * float * float

  let create_control_points { c; rx; ry } : float point * curve * curve =
    let { x; y } = c in
    let half_height = ry /. 2. in
    let width_two_thirds = rx *. (2. /. 3.) in
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
    begin_path ctx;
    let start, curve_one, curve_two = create_control_points ellipse in
    ctx##moveTo start.x start.y;
    let x1, y1, x2, y2, x3, y3 = curve_one in
    ctx##bezierCurveTo x1 y1 x2 y2 x3 y3;
    let x1, y1, x2, y2, x3, y3 = curve_two in
    ctx##bezierCurveTo x1 y1 x2 y2 x3 y3;
    ctx##stroke;
    ctx##moveTo 0. 0.;
    close_path ctx

  let rec split_at n lst =
    match (n, lst) with
    | 0, _ -> ([], lst)
    | _, [] -> ([], [])
    | n, x :: xs ->
        let taken, rest = split_at (n - 1) xs in
        (x :: taken, rest)

  let rec partition n ?(step = 0) lst =
    match lst with
    | [] -> []
    | lst ->
        let taken, _ = split_at n lst in
        if List.length taken = n then taken :: partition n ~step (List.tl lst)
        else []

  let draw_polygon ctx polygon =
    let points = partition 2 ~step:1 (polygon @ [ List.hd polygon ]) in
    begin_path ctx;
    List.iter
      (fun pair ->
        let Shape.{ x = x1; y = y1 }, Shape.{ x = x2; y = y2 } =
          (List.nth pair 0, List.nth pair 1)
        in
        ctx##moveTo x1 y1;
        ctx##lineTo x2 y2)
      points;
    ctx##stroke;
    close_path ctx

  let render_axes () =
    let tmap f (a, b) = (f a, f b) in
    let render_axes' { context = ctx; size; _ } =
      let fsize = tmap float_of_int size in
      let hw, hh = tmap (fun n -> n /. 2.) fsize in
      let w, h = fsize in
      ctx##beginPath;
      ctx##moveTo 0. hh;
      ctx##lineTo w hh;
      ctx##moveTo hw 0.;
      ctx##lineTo hw h;
      ctx##closePath
    in
    match !context with Some ctx -> render_axes' ctx | None -> fail ()

  let show shapes =
    let rec render' context = function
      | Shape.Circle circle' -> draw_circle context circle'
      | Shape.Ellipse ellipse' -> draw_ellipse context ellipse'
      | Shape.Line line' -> draw_line context line'
      | Shape.Polygon polygon' -> draw_polygon context polygon'
      | Shape.Complex complex' -> List.iter (render' context) complex'
    in
    match !context with
    | Some ctx -> List.iter (render' ctx.context) shapes
    | None -> fail ()
end
