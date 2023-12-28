open Joy

let () =
  init ();
  background (1., 1., 1., 1.);
  (* Create ellipse *)
  let e1 = ellipse 60. 30. in
  (* Translate it to the right by 100 and up by 50 *)
  let e2 = translate 100. 50. e1 in
  (* Display both ellipses *)
  set_color (0., 0., 0.);
  show [ e1; e2 ];
  write ~filename:"translate_ellipse.png" ()
