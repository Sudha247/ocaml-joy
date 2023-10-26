open Graphics

let () =
  open_graph "300x300";
  set_color black;

  let x = 150 in
  let y = 150 in
  let radius = 50 in

  (* Specify the angle range for a semi-circle (0 to 180 degrees) *)
  let start_angle = 0 in
  let end_angle = 180 in

 
  draw_arc x y radius start_angle end_angle;

  ignore (read_line ());
  close_graph ();
  exit 0
