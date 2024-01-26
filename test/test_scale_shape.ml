open Joy

let run () =
  init ();
  let c1 = circle 50 in
  let c2 = scale 2. c1 in
  let c3 = scale 0.5 c1 in
  let r1 = rectangle 100 100 |> translate 10 500 in
  let r2 = scale 2. r1 in
  let r3 = scale 0.02 r1 in
  let e1 = ellipse 30 50 |> translate 500 500 in
  let e2 = scale 2. e1 in
  let e3 = scale 0.7 e1 in
  show [ c1; c2; c3; r1; r2; r3; e1; e2; e3 ]
