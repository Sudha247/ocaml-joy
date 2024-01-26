open Joy

let () =
  init ();
  background (255, 255, 255, 255);
  let c = circle 50 in
  set_color (0, 0, 0);
  render c;
  write ~filename:"circle.png" ()
