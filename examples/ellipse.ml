open Joy

let () =
  init ();
  (* create an ellipse *)
  let e = ellipse 100 75 in
  (* render it *)
  render e;
  write ~filename:"ellipse.png" ()
