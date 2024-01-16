(* simple example demonstrating drawing x and y axes for debugging or plots *)

open Joy

let _ =
  (* intialize rendering context with the axes flag set to true *)
  init ~axes:true ();
  background (255, 255, 255, 255)
;;

(* set background to opaque white *)
let c = circle 50 in
set_color (0, 0, 0);
render c;
(* Write to PNG! *)
write ~filename:"axes.png" ()
