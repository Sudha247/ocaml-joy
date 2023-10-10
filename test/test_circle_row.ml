open Joy.Shape

let run ()=
  init ();
  let base_circle = circle 50 in
  let circle1 = translate 150 0 base_circle in
  let circle2 = translate (-150) 0 base_circle in
  show [base_circle; circle1; circle2];
  ignore (read_line ());
  close()
