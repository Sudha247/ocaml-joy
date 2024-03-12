type context =
    CairoContext of Backend_cairo.context
  | SVGContext of Backend_svg.context
  | LazyContext of Backend_lazy.context

exception No_context
exception Unsupported_output_format of string

let default = ref (LazyContext (Backend_lazy.create ()))

let get_default _ =
  !default

let set_default ctx =
  default := ctx

let show ?ctx shapes =
  let ctx = match ctx with
    | Some ctx -> ctx
    | None -> get_default ()
  in
  match ctx with
  | CairoContext ctx -> Backend_cairo.show ctx shapes
  | SVGContext ctx -> Backend_svg.show ctx shapes
  | LazyContext ctx -> Backend_lazy.show ctx shapes

let set_line_width ?ctx int =
  let ctx = match ctx with
  | Some ctx -> ctx
  | None -> get_default ()
  in
  match ctx with
  | CairoContext ctx -> Backend_cairo.set_line_width ctx int
  | SVGContext _ -> failwith "SVG.set_line_width ctx int"
  | LazyContext _ -> failwith "Backend_lazy.set_line_width ctx int"

let writePNG ?ctx filename =
  let ctx = match ctx with
    | Some ctx -> ctx
    | None -> get_default ()
  in
  match ctx with
  | CairoContext ctx -> Backend_cairo.write ctx filename
  | SVGContext _ -> raise (Unsupported_output_format "SVG context cannot render to PNG")
  | LazyContext _ -> failwith "Lazy.writePNG ctx filename"

let writeSVG ?ctx =
  let ctx = match ctx with
    | Some ctx -> ctx
    | None -> get_default ()
  in
  match ctx with
  | CairoContext _ -> raise (Unsupported_output_format "Cairo context cannot render to SVG")
  | SVGContext _ -> failwith "SVG.writeSVG ctx"
  | LazyContext _ -> failwith "Lazy.writeSVG ctx"

