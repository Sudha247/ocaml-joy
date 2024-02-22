open Joy

let _ =
  init ();
  let c = circle 50 |> with_stroke red in
  render c;
  write ~filename:"color.png" ()
