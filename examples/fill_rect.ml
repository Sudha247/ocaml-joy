open Joy

let () =
  init ();
  let r = rectangle 500 500 |> no_stroke |> with_fill (255, 0, 0) in
  show [ r ];
  write ~filename:"fill-rect.png" ()
