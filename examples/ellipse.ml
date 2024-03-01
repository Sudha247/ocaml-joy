open Joy

let () =
  init ();
  (* create an ellipse *)
  let e = ellipse 100 75 in
  (* render it *)
  show [ e ];
  write ~filename:"ellipse.png" ()
