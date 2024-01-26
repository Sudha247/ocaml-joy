open Joy

let () =
  init ~size:(500., 300.) ();
  background (1., 1., 1., 1.);
  let c = circle 50. in
  set_color (0., 0., 0.);
  show [ c ];
  write ~filename:"rectangular_canvas.png" ()
