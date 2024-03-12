open Joy

let _ =
  init ();
  let ell = ellipse 100 50 in
  show [ ell; ell |> rotate 60 ];
  write ~filename:"simple_rotate_ellipse.png" ()
