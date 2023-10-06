let () =
  init ();
  set_color black;

  (* Create circle transform *)
  let c = circle 100 in

  (* Display circle transform *)
  show [c];

  close ()