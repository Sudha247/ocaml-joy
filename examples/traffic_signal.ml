open Graphics

let draw_traffic_light () =
  open_graph " 500x500"; 

  (* Black rectangle *)
  set_color black;
  fill_rect 100 100 200 400;

  let circle_radius = 50 in  

  let gap = 110 in 

  (* Y-coordinates for the circles *)
  let red_y = 400 in
  let amber_y = red_y - gap in
  let green_y = amber_y - gap in

  (* Red circle *)
  set_color red;
  fill_circle 200 red_y circle_radius;

  (* Amber circle *)
  set_color yellow; 
  fill_circle 200 amber_y circle_radius;

  (* Green circle *)
  set_color green;
  fill_circle 200 green_y circle_radius;

  ignore (read_line ());
  close_graph ()

let () =
  draw_traffic_light ();
  print_endline "Traffic light illustration created and displayed!";