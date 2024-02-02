open Joy

let () =
  init ();
  background (255, 255, 255, 255);
  set_color (0, 0, 0);
  let r = rectangle 100 200 in
  show [ r ];
  write ~filename:"rectangle.png" ()
