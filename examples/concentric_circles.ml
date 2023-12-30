open Joy

let () =
  init ();
  background (1., 1., 1., 1.);

  let init_circle = circle 200. in
  let interval = 1. -. (1. /. 20.) in
  let rec make_concentric (arr : shape list) (i : int) : shape list =
    match (arr, i) with
    | [], 21 -> make_concentric [ init_circle ] 20
    | hd :: _, n when n > 0 ->
        make_concentric ([ scale interval hd ] @ arr) (n - 1)
    | _, _ -> arr
  in
  let circles = complex (make_concentric [] 21) in
  set_color (0., 0., 0.);
  render circles;
  write ~filename:"concentric_circles.png" ()
