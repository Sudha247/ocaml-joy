open Graphics

let () =
  open_graph " 300x300";
  set_color black;

  (* Draw an ellipse  *)
  draw_ellipse 150 150 50 30;

  ignore (read_line ());
  close_graph ();
  exit 0
