open Joy

let () =
  init "canvas";
  (* create an ellipse *)
  let e = ellipse 100 75 in
  (* render it *)
  show [ e ]
