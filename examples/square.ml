open Joy

let size = 100.

let () =
  init ();
  background (1., 1., 1., 1.);
  (* creating a rectangle from points *)
  let rect = rectangle size size in
  set_color (0., 0., 0.);
  render rect;
  write ~filename:"square.png" ()
