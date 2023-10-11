open Joy.Shape 

let _ = 
  init ();
  let rect = rectangle 100 100 in 
  render_shape rect;
  close ();
  exit 0