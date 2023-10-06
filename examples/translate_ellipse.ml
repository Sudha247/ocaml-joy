open Graphics

let translate_ellipse dx dy ellipse =
  match ellipse with
  | Ellipse e -> Ellipse { e with c = { x = e.c.x + dx; y = e.c.y + dy }; }

let () =
  init ();
  set_color black;

  (* Create ellipse transform *)
  let e = Ellipse_transform.translate_ellipse 50 (-50) (ellipse 40 20) in

  (* Display ellipse transform *)
  show [e];

  close ()