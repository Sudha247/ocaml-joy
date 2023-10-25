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
        rotate i rect)
      nums
  in
  show rotated;
  close ()