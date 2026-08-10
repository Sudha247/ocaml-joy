open Joy

let _ =
  init "canvas";
  let ell = ellipse 100 50 in
  show [ ell; ell |> rotate 60 ]
