open Joy

let size = 800
let interval = 16
let line_interval = 800 / interval
let rec range a b = if a > b then [] else a :: range (a + 1) b
let inc x = x + 1

let _ =
  init ~size:(size, size) ();
  let half_size = size / 2 in
  let lines =
    List.map
      (fun i ->
        let newx = i |> inc |> ( * ) line_interval in
        line
          ~a:(point (newx - half_size) (-half_size))
          (point (newx - half_size) half_size))
      (range 0 interval)
  in
  show lines;
  write ~filename:"parallel_lines.png" ()
