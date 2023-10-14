open Graphics

let () =
  open_graph " 500x500";
  
  set_color black;

  let draw_hexagon () =
    let points = 
      [|
        (132, 450);
        (60, 312);
        (132, 174);
        (368, 174);
        (440, 312);
        (368, 450);
      |] in
    draw_poly points
  in

  draw_hexagon();

  ignore (read_line ());
  close_graph ();
  exit 0