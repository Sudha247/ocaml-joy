open Joy.Canvas

let () =
  init ();
  background (255, 255, 255, 255);
  let c = circle 100 in
  set_color (0, 0, 0);
  show [ c ]
