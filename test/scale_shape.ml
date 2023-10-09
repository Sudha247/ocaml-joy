open Joy.Shape

let () =
  (* set_dimensions 100 100; *)
  init ();
  let c1 = circle 50 in
  let c2 = scale 2 c1 in
  let c3 = scale 3 c1 in
  let r1 = rectangle ~x:10 ~y:500 100 100 in
  let r2 = scale 2 r1 in
  let r3 = scale 3 r1 in
  let e1 = ellipse ~x:500 ~y:500 30 50 in
  let e2 = scale 2 e1 in
  let e3 = scale 3 e1 in
  show [ c1; c2; c3; r1; r2; r3; e1; e2; e3 ];
  close ();
  