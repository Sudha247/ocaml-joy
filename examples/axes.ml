(* simple example demonstrating drawing x and y axes for debugging or plots *)

open Joy

let _ =
  (* intialize rendering context with the axes flag set to true *)
  init ~axes:true ();
  (* set background to opaque white *)
  background (255, 255, 255, 255);
  let c = circle 50 in
  set_color (0, 0, 0);
  render c;
  (* Write to PNG! *)
  write ~filename:"axes.png" ()
