open Joy

let () =
  init ~axes:true ();
  let square = rectangle 100 100 in
  show [ square ];
  write ~filename:"square.png" ()
