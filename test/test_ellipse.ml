open Joy.Shape

let () =
  init ();
  let e1 = ellipse 100 200 in
  let e2 = ellipse 200 100 in
  let e3 = ellipse ~x:500 ~y:500 100 100 in
  show [ e1; e2; e3 ];
  close ();