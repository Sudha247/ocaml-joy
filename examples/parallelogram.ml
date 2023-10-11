open Graphics

let () =
  open_graph " 300x300";
  set_color black;
  draw_poly [| (100, 200); (200, 200); (250, 250); (150, 250) |];
  ignore (read_line ());
  close_graph ();
  exit 0;
