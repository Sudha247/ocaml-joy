open Joy

let max = 32.
let rec range a b = if a > b then [] else a :: range (a +. 1.) b

let _ =
  init ();
  let rect = rectangle 100 50 |> translate 195 220 in
  let ell = ellipse 100 50 |> translate 60 60 in
  let nums = range 0. max in
  let rotated =
    List.map (fun i -> rotate (int_of_float (i /. max *. 360.0)) ell) nums
  in
  show (rect :: rotated);
  write ~filename:"rotate_ellipse.png" ()
