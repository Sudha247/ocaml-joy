open Joy

let _ =
  init ();
  let l1 = line (point 50 50) in
  let l2 = line (point (-50) 50) in
  let l3 = line ~a:(point (-50) 50) (point 50 50) in
  show [l1; l2; l3];
  write ~filename:"line.png" ()

