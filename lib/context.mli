type joy_context = {
  ctx : Cairo.context;
  surface : Cairo.Surface.t;
  size : float * float;
  filename : string;
}

val context : joy_context option ref
val fail : unit -> unit

exception Context of string

val init_context : float -> float * float -> string -> unit
val get_window_size : unit -> float * float
val set_color : float * float * float -> unit
val background : float * float * float * float -> unit
val write : joy_context -> unit
