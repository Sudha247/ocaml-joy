open Joy.Svg

let () =
  init ();
  let r = rectangle 100 200 in
  show [ r ];
  write ~filename:"rectangle.png" ()
