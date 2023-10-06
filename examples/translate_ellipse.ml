let () =
  init ();
  set_color black;

  (* Create ellipse transform *)
  let e = ellipse 40 20 in

  (* Display ellipse transform *)
  show [e];

  close ()