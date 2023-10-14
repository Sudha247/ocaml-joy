open Graphics
let draw_square () =
  let points = 
    [|
      (100, 400);
      (200, 400);
      (200, 300);
      (100, 300);
    |] in
  draw_poly points

  let draw_triangle () =
    let points = 
      [|
        (300, 300);
        (400, 300);
        (350, 400);
      |] in
    draw_poly points
let draw_hexagon () =
  let points = 
    [|
      (266, 215);
      (230, 146);
      (266, 77);
      (384, 77);
      (420, 146);
      (384, 215);
    |] 
  in
  draw_poly points

  let draw_pentagon () =
    let points = 
      [|
        (125, 225);
        (30, 156);
        (66, 44);
        (184, 44);
        (220, 156)
      |] 
    in
    draw_poly points


let () =
  open_graph " 500x500";
  
  set_color black;
  draw_square();
  draw_hexagon();
  draw_pentagon();
  draw_triangle ();
 

  ignore (read_line ());
  close_graph ();
  exit 0