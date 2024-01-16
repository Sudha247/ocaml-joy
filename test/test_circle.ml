open Joy

let run () =
  init ();
  let c1 = circle 50 in
  let c2 = circle 100 in
  show [ c1; c2 ]
