open Joy

let () =
  init ();
  background (1., 1., 1., 1.);
  let e = ellipse 50. 75. in
  let shapes = repeat 16 (rotate (360. /. 64. |> int_of_float)) e in
  set_color (0., 0., 0.);
  render shapes;
  write ~filename:"ellipse_rotation.png" ()
