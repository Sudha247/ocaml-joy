open Joy

let outer_radius = 200.
let inner_radius = 80.
let points = 5
let angle_step = 2. *. Float.pi /. float_of_int points

let star_section i =
  let i = float_of_int i in
  let x = outer_radius *. cos (angle_step *. i)
  and y = outer_radius *. sin (angle_step *. i) in
  let outer_point = point (int_of_float x) (int_of_float y) in
  let x = inner_radius *. cos ((i +. 0.5) *. angle_step)
  and y = inner_radius *. sin ((i +. 0.5) *. angle_step) in
  [ outer_point; { x; y } ]

let () =
  init ();
  background (255, 255, 255, 255);
  set_line_width 3;
  let star = List.init points star_section |> List.flatten |> polygon in
  set_color (0, 0, 0);
  render star;
  write ~filename:"star.png" ()
