open Joy

let make_nose () =
  let l = line (point 0 50) |> translate 0 (-25) in
  let l2 = l |> rotate 90 |> translate 0 (-25) in
  complex [ l; l2 ]

let make_arc rx ry =
  let r = rectangle ~c:(point 0 (ry / 2)) (2 * rx) ry in
  let col = r |> with_fill white |> with_stroke white in
  let e = ellipse ~c:(point 0 0) rx ry in
  complex [ e; col ]

let () =
  init "canvas";
  let face = circle 200 in
  let left_eye = circle ~c:(point (-50) 50) 20 in
  let right_eye = circle ~c:(point 50 50) 20 in
  let nose = make_nose () in
  let left_brow = make_arc 26 14 |> rotate 180 |> translate (-50) 70 in
  let right_brow = make_arc 26 14 |> rotate 180 |> translate 50 70 in
  let mouth = make_arc 80 40 |> translate 0 (-60) in
  show [ mouth; left_brow; right_brow; face; left_eye; right_eye; nose ]
