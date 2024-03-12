open Joy

let make_nose () =
    let l = line (point 0 50)|> translate 0 (-25) in
    let l1 = l in
    let l2 = l |> rotate 90|> translate 0 (-25) in
    let nose = complex [l1; l2] in
    nose

let make_arc rx ry = 
    let r = rectangle ~c:(point 0 (ry / 2)) (2 * rx) ry in 
    let col = r |> with_fill white |> with_stroke white in
    let e = ellipse ~c:(point 0 0) rx ry in
    complex [e; col]

let () =
    init ();
    let a = circle 200 in
    let d = circle ~c:(point 50 50) 20 in
    let b = circle ~c:(point (-50) 50) 20 in
    let nose = make_nose () in
    let leb = make_arc 26 14 |> rotate 180 |> translate 50 70  in
    let reb = make_arc 26 14 |> rotate 180 |> translate (-50) 70  in 
    let mouth = make_arc 80 40 |>translate 0 (-60) in 
    show [mouth;leb;reb;a;d;b;nose];
    write ()

