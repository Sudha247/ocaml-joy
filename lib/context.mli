type context =
  | CairoContext of Backend_cairo.context
  | SVGContext of Backend_svg.context
  | LazyContext of Backend_lazy.context

exception No_context
exception Unsupported_output_format of string

val get_default : unit -> context
val set_default : context -> unit
val show : ?ctx:context -> Shape.shape list -> unit
val clear : ?ctx:context -> unit -> unit
val set_line_width : ?ctx:context -> int -> unit

val makeSVG : ?ctx:context -> unit -> string
val writePNG : ?ctx:context -> string -> unit
