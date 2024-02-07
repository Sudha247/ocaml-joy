open Joy.Svg

let () =
  init ();
  background (255, 255, 255, 255);
  (* Create rectangle transformation *)
  let r1 = rectangle 200 100 in
  let r2 = translate 100 0 r1 in

  (* Display rectangle transformation *)
  set_color (0, 0, 0);
  show [ r1; r2 ];
  write ~filename:"translate_rectangle.png" ()
