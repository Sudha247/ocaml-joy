open Joy.Canvas

let () =
  init ~size:(400, 400) ();
  let c = circle 100 in
  let r = rectangle 200 150 in
  show [ c; r ]
