open Graphics

let () =
  open_graph " 300x300";
  (* Open a graphics window with dimensions 800x600 *)
  set_color black;

  draw_circle 150 150 50;

  ignore (read_line ());
  close_graph ();
  exit 0