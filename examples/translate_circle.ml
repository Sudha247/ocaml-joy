open Joy

let () =
  init ();
  background (1., 1., 1., 1.);
  (* Create circle *)
  let c1 = circle 100. in
  (* Translate it to the right by 100 *)
  let c2 = translate 100. 0. c1 in
  (* Display both circles *)
  set_color (0., 0., 0.);
  show [ c1; c2 ];
  write ~filename:"translate_circle.png" ()
