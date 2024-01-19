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

let init_context line_width (w, h) axes =
  (* Fail if context has already been instantiated *)
  if Option.is_some !context then
    raise (Context "Cannot initialize context twice");

  let surface = Cairo.Image.create Cairo.Image.ARGB32 ~w ~h in
  let ctx = Cairo.create surface in
  Cairo.set_line_width ctx line_width;
  Cairo.translate ctx (w / 2 |> float_of_int) (h / 2 |> float_of_int);
  context := Some { ctx; surface; size = (w, h); axes }

let resolution () = match !context with Some ctx -> ctx.size | None -> fail ()
let tmap3 f (a, b, c) = (f a, f b, f c)
let tmap4 f (a, b, c, d) = (f a, f b, f c, f d)
let ( >> ) f g x = g (f x)
let scale_color_channel x = x /. 256.
let scale_color_channel = float_of_int >> scale_color_channel

let tmap3 f (a, b, c) = (f a, f b, f c) 
let tmap4 f (a, b, c, d) = (f a, f b, f c, f d)
let scale_channel n = float_of_int n /. 255.
let set_color color =
  match !context with
  | Some ctx ->
      let r, g, b = tmap3 scale_color_channel color in
      Cairo.set_source_rgb ctx.ctx r g b
  | None -> fail ()

(* sets background color *)
let background color =
  match !context with
  | Some { ctx; _ } ->
      let r, g, b, alpha = tmap4 scale_color_channel color in
      Cairo.set_source_rgb ctx r g b;
      Cairo.paint ctx ~alpha;
      Cairo.fill ctx
  | None -> fail ()

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

let init_context background_color line_width (x, y) axes =
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
  context := Some { ctx; surface; size = (x, y); axes };
  background background_color