open Joy.Shape 

let _ = 
  draw_axes true;
  init ();
  let c = circle 50 in 
  render_shape c;
  close ();
  exit 0