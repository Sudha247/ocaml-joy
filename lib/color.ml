type color = int * int * int * float

(** RGBA constant to set transparent background *)
let transparent = (0, 0, 0, 0.0)

(** Converts RGB color into opaque RGBA color. 
    For use w/ `Context.background` *)
let rgb r g b = (r, g, b, 1.0)

(** RGB code for black *)
let black = rgb 0 0 0

(** RGB code for white *)
let white = rgb 255 255 255

(** RGB code for red *)
let red = rgb 255 1 1

(** RGB code for green *)
let green = rgb 1 255 1

(** RGB code for blue *)
let blue = rgb 1 1 255

(** RGB code for yellow *)
let yellow = rgb 255 255 255
