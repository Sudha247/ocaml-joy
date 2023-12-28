(* Global rendering context singleton definition and instantiation *)
type context = {
  ctx : Cairo.context;
  surface : Cairo.Surface.t;
  size : float * float;
  axes: bool;
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

let init_context line_width (x, y) axes =
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
  context := Some { ctx; surface; size = (x, y); axes }

let resolution () = match !context with Some ctx -> ctx.size | None -> fail ()

let set_color color =
  match !context with
  | Some ctx ->
      let r, g, b = color in
      Cairo.set_source_rgba ctx.ctx r g b 1.
  | None -> fail ()

(* sets background color *)
let background color =
  match !context with
  | Some ctx ->
      let r, g, b, a = color in
      Cairo.set_source_rgba ctx.ctx r g b a;
      Cairo.paint ctx.ctx
  | None -> fail ()

let set_line_width line_width =
  match !context with
  | Some ctx -> Cairo.set_line_width ctx.ctx line_width
  | None -> fail ()

let save () =
  match !context with Some ctx -> Cairo.save ctx.ctx | None -> fail ()

let restore () =
  match !context with Some ctx -> Cairo.restore ctx.ctx | None -> fail ()
