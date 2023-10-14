open Graphics

let () =
  open_graph " 500x500";
  
  set_color black;

  let draw_square () =
    let points = 
      [|
        (250, 450);
        (350, 450);
        (350, 350);
        (250, 350);
      |] in
    draw_poly points
  in

  draw_square();

  ignore (read_line ());
  close_graph ();
  exit 0