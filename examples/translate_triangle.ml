open Graphics

let translate_triangle dx dy triangle =
  match triangle with
  | Triangle t ->
      Triangle
        {
          p1 = { x = t.p1.x + dx; y = t.p1.y + dy };
          p2 = { x = t.p2.x + dx; y = t.p2.y + dy };
          p3 = { x = t.p3.x + dx; y = t.p3.y + dy };
        }

let () =
  init ();
  set_color black;
      
  (* Create triangle transform *)
  let t = Triangle_transform.translate_triangle 50 (-50) (triangle ~x1:50 ~y1:50 ~x2:100 ~y2:150 ~x3:150 ~y3:50 ()) in
      
  (* Display triangle transform *)
  show [t];
      
  close ()