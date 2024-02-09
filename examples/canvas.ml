open Joy.Canvas

let () =
  print_endline "TEST";
  init ~size:(400, 400) ();
  background (255, 255, 255, 255);
  let c = circle 100 in
  let r = rectangle 200 150 in
  set_color (255, 0, 0);
  show [ c; r ]
