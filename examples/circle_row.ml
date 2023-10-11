open Joy.Shape

let rec range a b = if a > b then [] else a :: range (a + 1) b

let _ =
  init ();
  let center = 250 in
  let radius = 50 in 
  let idx_to_circle i = 
    (circle ~x: (radius + center + (i - 2) * radius * 2) ~y: center radius) in
  let circles = List.map idx_to_circle (range 0 3) in
  show circles;
  close ()
