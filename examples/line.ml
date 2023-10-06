open Graphics
open Joy.Shape 

let quarter_size = (size_x ()) / 4
let rec range a b = if a > b then [] else a :: range (a + 1) b
let lines = List.map (fun i -> let newx = (i |> ( + ) 1 |> ( * ) quarter_size) in (line ~x1:newx ~y1:0 newx (size_y ()))) (range 0 3)

let _ = 
  init ();
  List.iter render_shape lines;
  close ();
  exit 0