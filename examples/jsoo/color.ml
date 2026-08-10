open Joy

let _ =
  init "canvas";
  let c = circle 50 |> with_stroke red in
  show [ c ]
