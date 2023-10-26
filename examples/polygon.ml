open Joy.Shape 

let rec range a b = if a > b then [] else a :: range (a +. 1.0) b
let num_points = 5.0
let default_size = 50

let idx_to_point i = 
  let x = (cos (i /. num_points) *. Float.pi) in 
  let y = (sin (i /. num_points) *. Float.pi) in 
  {x = (int_of_float x) * default_size; y = (int_of_float y) * default_size}

let () = 
  init ();
  let points = List.map idx_to_point (range 0.0 num_points) in 
  (* let points = [{x = 0; y = 0}; {x = 0; y = 100}; {x = 100; y = 100;}; {x = 100; y = 0}] in *)
  let poly = polygon points in 
  render_shape poly;
  close ()