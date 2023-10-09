open Graphics

let translate_circle dx dy circle =
  match circle with
  | Circle c -> Circle { c with c = { x = c.c.x + dx; y = c.c.y + dy }; }

let () =
  init ();
  set_color black;

  (* Create circle transform *)
  let c = Circle_transform.translate_circle 100 0 (circle 100) in

  (* Display circle transform *)
  show [c];

  close ()