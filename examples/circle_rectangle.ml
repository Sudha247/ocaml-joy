open Joy.Shape

let () =
  init ();

  let circle = circle 50 in 
  let rectangle = rectangle ~x: 200 ~y: 200 100 100 in 
  show [circle; rectangle];

  close ()
