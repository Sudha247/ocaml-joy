let () =
  init ();
  set_color black;

  (* Create shapes *)
  let r1 = rectangle 200 100 in
  let r2 = translate 100 0 r1 in
  let t = triangle ~x1:50 ~y1:50 ~x2:100 ~y2:150 ~x3:150 ~y3:50 () in

  (* Display shapes *)
  show [r1; r2; t];

  close ()
