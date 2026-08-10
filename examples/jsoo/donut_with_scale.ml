open Joy

let () =
  init "canvas";
  let c = circle 100 in
  let hole = scale 0.5 c in
  show [ c; hole ]
