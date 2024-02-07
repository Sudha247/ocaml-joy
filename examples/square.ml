open Joy

let () =
  init ();
  background (255, 255, 255, 255);
  let square = rectangle 100 100 in
  set_color (0, 0, 0);
  show [ square ];
  write ~filename:"square.png" ()
