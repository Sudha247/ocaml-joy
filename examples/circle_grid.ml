open Graphics

let () =
  open_graph " 400x400 ";  (* Open a graphics window with dimensions 400x400 *)
  set_color black;

  let circle_radius = 70 in
  let gap_between_circles = 0 in
  let circle_diameter = 2 * circle_radius in

(*calculating the positions of the four circles*)
  let cx1 = circle_radius + gap_between_circles in
  let cy1 = circle_radius + gap_between_circles in

  let cx2 = cx1 + circle_diameter in
  let cy2 = cy1 in

  let cx3 = cx1 in
  let cy3 = cy1 + circle_diameter in

  let cx4 = cx2 in
  let cy4 = cy3 in

  (* Draw circles *)
  draw_circle cx1 cy1 circle_radius;
  draw_circle cx2 cy2 circle_radius;
  draw_circle cx3 cy3 circle_radius;
  draw_circle cx4 cy4 circle_radius;

  ignore (read_line ());
  close_graph ();
  exit 0
