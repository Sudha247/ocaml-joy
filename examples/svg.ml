open Joy

let _ =
  init_svg ();
  show [ circle 50; rectangle 100 100 ];
  Printf.printf "%s\n" (Joy.Context.writeSVG ())

