let () =
  init ();
  set_color black;
      
  (* Create triangle transform *)
  let t = triangle ~x1:50 ~y1:50 ~x2:100 ~y2:150 ~x3:150 ~y3:50 () in
      
  (* Display triangle transform *)
  show [t];
      
  close ()