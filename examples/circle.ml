open Joy.Svg

let () =
  init ();
  let c = circle 50 in
  show [ c ];
  write ~filename:"circle.png" ()
