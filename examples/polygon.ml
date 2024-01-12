open Joy

let size = 100.

let () =
  init ();
  background (1., 1., 1., 1.);
  let poly =
    polygon
      [ { x = -.size; y = 0. }; { x = 0.; y = size }; { x = size; y = 0. } ]
  in
  set_color (0., 0., 0.);
  render poly;
  write ~filename:"polygon.png" ()
