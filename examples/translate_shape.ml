open Graphics
open Shapes

let show shapes =
  open_graph " 500x500";
  set_color black;
  List.iter render_shape shapes;
  ignore (read_line ());
  close_graph ()

let () =
  let c = circle 100 100 30 in
  let e = ellipse ~x:200 ~y:200 40 20 in
  let r1 = rectangle 200 100 in
  let r2 = translate 100 0 r1 in
  let t = triangle ~x1:50 ~y1:50 ~x2:100 ~y2:150 ~x3:150 ~y3:50 () in
  let s = star ~x:350 ~y:350 30 60 5 in

  show [r1; r2; t; s; c; e]
