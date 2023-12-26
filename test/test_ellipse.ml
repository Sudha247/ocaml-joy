open Joy

let run () =
  init ();

  let e1 = ellipse 50. 30. in
  let e2 = ellipse 100. 60. in

  show [ e1; e2 ]
