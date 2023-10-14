open Graphics

let () =
  open_graph " 500x500";
  
  set_color black;

  let draw_pentagon () =
    let points = 
      [|
        (250, 450);
        (60, 312);
        (132, 88);
        (368, 88);
        (440, 312);
      |] in
    Graphics.draw_poly points
  in

  draw_pentagon();

  ignore (read_line ());
  close_graph ();
  exit 0