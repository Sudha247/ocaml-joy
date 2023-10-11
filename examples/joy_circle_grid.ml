open Joy.Shape

let grid_of_circles canvas_width canvas_height spacing =
  let create_circle x y =
    circle 50 |> translate x y
  in
  let num_circles_x = canvas_width / spacing in
  let num_circles_y = canvas_height / spacing in
  let initial_x = (canvas_width - num_circles_x * spacing) / 2 in
  let initial_y = (canvas_height - num_circles_y * spacing) / 2 in

  let circles = 
    List.flatten
      (List.map
         (fun i ->
           List.map
             (fun j ->
               create_circle (initial_x + i * spacing) (initial_y + j * spacing))
             (List.init num_circles_y (fun x -> x)))
         (List.init num_circles_x (fun x -> x)))
  in

  show circles

let () =
  init ();
  let canvas_width = 800 in
  let canvas_height = 600 in
  let spacing = 150 in

  grid_of_circles canvas_width canvas_height spacing;

  ignore (read_line ());
   close ()
