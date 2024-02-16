type color = int * int * int

(** RGB code for black *)
let black = (0, 0, 0)

(** RGB code for white *)
let white = (255, 255, 255)

(** RGB code for red *)
let red = (255, 1, 1)

(** RGB code for green *)
let green = (1, 255, 1)

(** RGB code for blue *)
let blue = (1, 1, 255)

(** RGB code for yellow *)
let yellow = (255, 255, 1)

(** RGBA constant to set transparent background *)
let transparent = (0, 0, 0, 0)

(** Converts RGB color into opaque RGBA color. 
    For use w/ `Context.background` *)
let opaque (r, g, b) = (r, g, b, 255)
