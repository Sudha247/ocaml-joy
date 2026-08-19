open Joy

let () =
  init "canvas";
  let r = rectangle 200 200 |> with_fill (rgb 255 0 0) in
  show [ r ]
