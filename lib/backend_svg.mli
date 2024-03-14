type context = {
  shapes: Shape.shape list ref;
  size: int * int;
  axes: bool;
  elt: Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t;
}

val show : context -> Shape.shape list -> unit
val create : size:int * int -> axes:bool -> eltId:string -> context
val make_svg : context -> string
val write : context -> unit
val clear : context -> unit
