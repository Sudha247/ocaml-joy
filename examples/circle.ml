open Joy.Shape

let _ =
  init ();
  let c = circle 75 in 
  render_shape c;
  close ();
  exit 0
