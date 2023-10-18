open Joy.Shape

let canvas_size = (500, 500)

let grid_of_circles (canvas_width, canvas_height) spacing =
  let create_circle x y = circle 50 |> translate x y in
  let num_circles_x = canvas_width / spacing in
  let num_circles_y = canvas_height / spacing in
  let initial_x = (canvas_width - (num_circles_x * spacing)) / 2 in
  let initial_y = (canvas_height - (num_circles_y * spacing)) / 2 in


  let circles =
    List.flatten (List.init num_circles_x (fun i ->
      List.init num_circles_y (fun j ->
        let x_position = initial_x + (i * spacing) in
        let y_position = initial_y + (j * spacing) in
        create_circle x_position y_position
      )
    ))
  in 

  show circles

let () =
  init ();
  grid_of_circles canvas_size 150;
  ignore (read_line ());
  close ()

