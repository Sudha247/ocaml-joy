open Joy.Svg

let () =
  init ~size:(500, 300) ();
  let c = circle 50 in
  show [ c ];
  write ~filename:"rectangle_canvas.png" ()
