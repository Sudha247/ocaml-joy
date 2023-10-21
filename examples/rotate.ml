open Joy.Shape

let max = 32
let rec range a b = if a > b then [] else a :: range (a + 1) b

let _ =
  init ();
  let rect = rectangle 10 10 in
  let nums = range 0 max in
  let rotated =
    List.map
      (fun i ->
        rotate (int_of_float (float_of_int i /. float_of_int max *. 360.0)) rect)
      nums
  in
  show rotated;
  close ()
