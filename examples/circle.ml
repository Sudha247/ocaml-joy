open Joy

let () =
  init ();
  let c = circle 50 in
  render c;
  write ~filename:"circle.png" ()
