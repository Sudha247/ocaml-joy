open Joy.Svg

let size = 100.

let () =
  init ();
  let poly =
    polygon
      [ { x = -.size; y = 0. }; { x = 0.; y = size }; { x = size; y = 0. } ]
  in
  show [ poly ];
  write ~filename:"polygon.png" ()
