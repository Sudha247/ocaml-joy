open Graphics

let () =
open_graph " 300x300";
set_color black;

let x = 150 in
let y = 150 in
let radius = 50 in

(* Draw a full circle *)
draw_circle x y radius;

(* Calculate the coordinates for the rectangle to erase the unwanted part *)
let rect_x = x - radius in
let rect_y = y in
let rect_width = 2 * radius in
let rect_height = radius in

(* Erase the upper part of the circle to create a semi-circle effect *)
set_color Graphics.background;
fill_rect rect_x rect_y rect_width rect_height;

(* Reset the color to black for future drawing *)
set_color black;

ignore (read_line ());
  close_graph (); 
  exit 0


