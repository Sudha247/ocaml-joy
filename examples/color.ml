open Joy

let _ =
  init ();
  let c = circle 50 |> with_stroke red in
  show [ c ];
  write ~filename:"color.png" ()
