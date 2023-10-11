open Joy.Shape

let draw_triangle () = 
  let p1 = (250, 400) in 
  let p2 = (150, 200) in 
  let p3 = (350, 200) in 
  let points = [(p1, p2); (p2, p3); (p3, p1)] in 
  let lines = List.map (fun ((a, b), (c, d)) -> line ~x1: a ~y1: b c d) points in
  show lines

let _ = 
  init ();
  draw_triangle ();
  close ();
  exit 0