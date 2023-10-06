open Graphics

let translate_rectangle dx dy rectangle =
  match rectangle with
  | Rectangle r -> Rectangle { r with c = { x = r.c.x + dx; y = r.c.y + dy }; }

let () =
  init ();
  set_color black;

  (* Create rectangle transform *)
  let r1 = rectangle 200 100 in
  let r2 = Rectangle_transform.translate_rectangle 100 0 r1 in

  (* Display rectangle transform *)
  show [r1; r2];

  close ()