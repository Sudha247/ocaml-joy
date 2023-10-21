open Joy.Shape

let run () =
  init ();

  let r1 = rectangle 50 30 in
  let r2 = rectangle 100 60 in

  show [ r1; r2 ];

  close ()
