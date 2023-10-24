open Joy.Shape

let max = 32
let rec range a b = if a > b then [] else a :: range (a + 1) b

let _ =
  init ();
  let ellipse = ellipse 20 10 in
  let nums = range 0 max in
  let rotated =
    List.map
      (fun i ->
        rotate (float_of_int i /. float_of_int max *. 360.0) ellipse)
      nums
  in
  show rotated;
  close ()