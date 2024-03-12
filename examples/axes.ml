(* simple example demonstrating drawing x and y axes for debugging or plots *)
open Joy

let _ =
  init ~axes:true ();
  let c = circle 50 in
  show [ c ];
  (* Write to PNG! *)
  write ~filename:"axes.png" ()
