open Graphics

let () =
  open_graph " 300x300";

  set_color black;

  let vertices = [|(100, 200); (200, 200); (250, 250); (150, 300); (100, 250)|] in
  draw_poly vertices;

  ignore (read_line ());
  close_graph ();
  exit 0
