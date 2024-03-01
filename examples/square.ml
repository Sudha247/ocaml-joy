open Joy

let () =
  init ();
  let square = rectangle 100 100 in
  show [ square ];
  write ~filename:"square.png" ()
