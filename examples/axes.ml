(* simple example demonstrating drawing x and y axes for debugging or plots *)

open Joy.Svg

let _ =
  (* intialize rendering context with the axes flag set to true *)
  init ~axes:true ();
  (* set background to opaque white *)
  let c = circle 50 in
  show [ c ];
  (* Write to PNG! *)
  write ~filename:"axes.png" ()
