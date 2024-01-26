open Joy

let size = 100.

let () =
  init ();
  background (255, 255, 255, 255);

  let triangle =
    polygon
      [ { x = -.size; y = 0. }; { x = 0.; y = size }; { x = size; y = 0. } ]
  in
  set_color (0, 0, 0);
  render triangle;
  write ~filename:"triangle.png" ()
