open Graphics

let () =
  open_graph " 8000x600 ";   (* Open a graphics window with dimensions 300x300 *)
  set_color black;

  (* Draw a rectangle*)
  let x = 100 in
  let y = 100 in
  let width = 200 in
  let height = 100 in
  draw_rect x y (x + width) (y + height);

  ignore (read_line ());
  close_graph ();
  exit 0
