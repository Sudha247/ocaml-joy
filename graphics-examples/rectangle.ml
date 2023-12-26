open Joy.Shape

let size = 100

let () =
  init ();
  (* creating a rectangle from points *)
  let rect = rectangle size size in
  render_shape rect;
  close ()
