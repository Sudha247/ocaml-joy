open Joy.Svg

let () =
  init ();
  background (255, 255, 255, 255);
  let c = circle 50 in
  set_color (0, 0, 0);
  show [ c ];
  write ~filename:"circle.png" ()
