open Joy.Shape

let () =
  init ();
  let e1 = triangle 200 250 300 350 300 500 in
  let e2= triangle 0 0 400 400 200 330 in
  let e3= triangle 250 400 150 200 350 200 in
  show [e1; e2; e3];
  close ();