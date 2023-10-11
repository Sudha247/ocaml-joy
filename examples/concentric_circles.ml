open Joy.Shape 

let rec range a b = if a > b then [] else a :: range (a + 1) b

let _ = 
  init ();
  let radius = 50 in
  let circles = List.map  (fun i -> circle (i * radius)) (range 1 4) in 
  show circles;
  close ()
