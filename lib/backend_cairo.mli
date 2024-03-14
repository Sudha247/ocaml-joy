type context = { dummy: string; size: int * int }

val create : background_color:int * int * int * float ->
            size:int*int -> line_width:int -> axes:bool -> context
val show : context -> Shape.shape list -> unit
val set_line_width : context -> int -> unit
val write : context -> string -> unit
val clear : context -> unit
