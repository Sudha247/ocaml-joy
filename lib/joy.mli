type 'a point = 'a Shape.point
(** A point in 2d space 
    generic but library functions require numeric types *)

type shape = Shape.shape
(** Shape variant type, this is what all shape constructor functions return.
    Contains types that are only meant for internal use, you should never need 
    to construct this type yourself.
    *)

type shapes = Shape.shapes
(** A list of {!shape} *)

type transformation = Transform.transformation
(** A function that takes a {!shape} and returns that 
    {!shape} with an operation applied to at least one field *)

type color = Color.color
(** Represents an RGB color *)

val point : int -> int -> float point
(** {!point} constructor function *)

val circle : ?c:float point -> int -> shape
(** {!circle} constructor function *)

val rectangle : ?c:float point -> int -> int -> shape
(** {!rectangle} constructor function *)

val ellipse : ?c:float point -> int -> int -> shape
(** {!ellipse} constructor function *)

val line : ?a:float point -> float point -> shape
(** {!line} constructor function *)

val polygon : float point list -> shape
(** {!polygon} consructor function *)

val complex : shapes -> shape
(** {!complex} constructor function *)

val with_stroke : color -> shape -> shape
(** Creates a new {!shape} with stroke [color] *)

val with_fill : color -> shape -> shape
(** Creates a new {!shape} with fill [color], does not affect {!line} *)

val rotate : int -> transformation
(** Rotates {!shape} by n degrees *)

val translate : int -> int -> transformation
(** Adds {[ { dx; dy } ]} to {!shape}'s position *)

val scale : float -> transformation
(** Multiplies {!shape}'s size by scaling factor *)

val compose : transformation -> transformation -> transformation
(** Composes two {!transformation}, returning a function that executes 
      both, feeding the result of the first into the second *)

val repeat : int -> transformation -> transformation
(** Iterative transformation - applies {!transformation} to {!shape} n 
      times, feeding the result into the next iteration *)

val map_stroke : (color -> color) -> shape -> shape
(** Applies f to stroke of {!shape} *)

val map_fill : (color -> color) -> shape -> shape
(** Applies f to fill of {!shape}, does not affect {!line} *)

val set_line_width : int -> unit
(** Sets the width of lines for both stroke of shapes and line primitives. 
    Can be any positive integer, with larger numbers producing thicker lines. 
    default is 2 *)

val black : color
(** RGB code for black *)

val white : color
(** RGB code for white *)

val red : color
(** RGB code for red *)

val green : color
(** RGB code for green *)

val blue : color
(** RGB code for blue *)

val yellow : color
(** RGB code for yellow *)

val transparent : int * int * int * int
(** RGBA code for a transparent color to use when setting the background color of sketches *)

val opaque : color -> int * int * int * int
(** Takes an RGB color and returns an opaque RGBA color *)

val init :
  ?background:color ->
  ?line_width:int ->
  ?size:int * int ->
  ?axes:bool ->
  unit ->
  unit
(** Initializes drawing context, takes optional arguments to set background color,
    line width, and resolution(size) *)

val render : shape -> unit

val show : shapes -> unit
(** Renders a list of shapes to canvas *)

val write : ?filename:string -> unit -> unit
(** Writes the current digital canvas to a PNG file, takes a filename 
    (i.e. "joy.png") as an optional argument *)
