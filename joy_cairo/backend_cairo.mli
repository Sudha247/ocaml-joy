type context
(** Opaque Cairo rendering context. *)

val create :
  ?background_color:Joy_core.Color.color ->
  ?line_width:int ->
  ?size:int * int ->
  unit ->
  context

val render : context -> Joy_core.Shape.shape list -> unit
val repaint_background : context -> unit
val set_line_width : context -> int -> unit
val write : context -> string -> unit
