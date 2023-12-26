open Joy

let interval = 16.
let line_interval = 800. /. interval
let rec range a b = if a > b then [] else a :: range (a +. 1.) b
let inc x = x +. 1.

let _ =
  init ();
  background(1., 1., 1., 1.);
  let lines =
    List.map
      (fun i ->
        let newx = i |> inc |> ( *. ) line_interval in
        line ~point:(point newx 0.) (point newx 800.))
      (range 0. interval)
  in
  set_color (0. ,0. ,0. );
  show lines
