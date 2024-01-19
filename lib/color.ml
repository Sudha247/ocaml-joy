type color = int * int * int

(** RGB code for black *)
let black = (0, 0, 0)

(** RGB code for white *)
let white = (255, 255, 255)

(** RGBA color constant to set transparent background *)
let transparent = (0, 0, 0, 0)

(** Converts RGB color into opaque RGBA color. 
    For use w/ `Context.background` *)
let opaque (r, g, b) = 
  (r, g, b, 255)

