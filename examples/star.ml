open Joy.Shape 

let center = (250, 250)
let num_points = 5
let outer = 100
let inner = 35

let deg_to_rad angle = angle *. Float.pi /. 180.0
let get_pos angle radius = 
  let x = (fst center) + int_of_float (radius *. cos (deg_to_rad angle)) in 
  let y = (snd center) + int_of_float (radius *. sin (deg_to_rad angle)) in 
  (x, y)

let draw_star () =
  let step = 360.0 /. float_of_int num_points in
  Graphics.moveto ((fst center) + outer) (snd center);
  for i = 1 to num_points * 2 do 
    let angle = float_of_int i *. step in 
    let radius = float_of_int (if i mod 2 = 0 then outer else inner) in 
    let (x, y) = get_pos angle radius in 
    Graphics.lineto x y 
  done; 
  Graphics.lineto ((fst center) + outer) (snd center)



let _ = 
  init ();
  draw_star ();
  close ();
  exit 0
