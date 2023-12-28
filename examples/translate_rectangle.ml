open Joy

let () =
  init ();
  background (1., 1., 1., 1.);
  (* Create rectangle transform *)
  let r1 = rectangle 200. 100. in
  let r2 = translate 100. 0. r1 in

  (* Display rectangle transform *)
  set_color (0., 0., 0.);
  show [ r1; r2 ];
  write ~filename:"translate_rectangle.png" ()
