open Joy.Shape 

let interval = 16
let line_interval = 500 / interval 
let rec range a b = if a > b then [] else a :: range (a + 1) b
let inc x = x + 1
let lines = List.map (fun i -> let newx = (i |> inc |> ( * ) line_interval) in (line ~x1:newx ~y1:0 newx 500)) (range 0 interval)

let _ = 
  init ();
  show lines;
  close ();
  exit 0