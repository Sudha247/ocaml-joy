open Joy

let size = 100.

let () =
  init "canvas";
  let poly =
    polygon
      [ { x = -.size; y = 0. }; { x = 0.; y = size }; { x = size; y = 0. } ]
  in
  show [ poly ]
