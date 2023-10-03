open Graphics

let () =
  open_graph " 400x400";
  (* Open a graphics window with dimensions 800x600 *)
  set_color black;

  draw_circle 200 200 50;
  draw_ellipse 200 200 200 50;

  ignore (read_line ());
  close_graph ();
  exit 0