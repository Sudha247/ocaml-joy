open Joy

let size = 800.
let interval = 16.
let line_interval = 800. /. interval
let rec range a b = if a > b then [] else a :: range (a +. 1.) b
let inc x = x +. 1.

let _ =
  init ~size: (size, size) ();
  background (1., 1., 1., 1.);
  let lines =
    List.map
      (fun i ->
        let newx = i |> inc |> ( *. ) line_interval in
        line ~point:(point (newx -. 400.) (-400.)) (point (newx -. 400.) 400.))
      (range 0. interval)
  in
  set_color (0., 0., 0.);
  show lines;
  write ~filename:"line.png" ()
