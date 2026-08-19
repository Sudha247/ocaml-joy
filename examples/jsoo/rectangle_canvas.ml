open Joy

let () =
  init ~size:(500, 300) "canvas";
  let c = circle 50 in
  show [ c ]
