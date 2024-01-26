open Joy

let () =
  init ();
  background (255, 255, 255, 255);
  let base_circle = circle 50 in
  let circle1 = base_circle |> translate (-100) 0 in
  let circle2 = base_circle |> translate 100 0 in
  set_color (0, 0, 0);
  show [ circle1; base_circle; circle2 ];
  write ~filename:"circle_row.png" ()
