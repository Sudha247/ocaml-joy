open Joy.Svg

let () =
  init ();
  let c = circle 100 in
  let hole = scale 0.5 c in
  show [ c; hole ];
  write ~filename:"donut_with_scale.png" ()
