open Joy.Shape

let grid_of_circles spacing =
  let create_circle x y =
    circle ~x:x ~y:y 50
  in
  let circles = ref [] in
  let half_spacing = spacing / 2 in
  let x_positions = [250 - half_spacing; 250 + half_spacing] in
  let y_positions = [250 - half_spacing; 250 + half_spacing] in
  for x = 0 to 1 do
    for y = 0 to 1 do
      let circle_x = List.nth x_positions x in
      let circle_y = List.nth y_positions y in
      circles := create_circle circle_x circle_y :: !circles
    done
  done;
  !circles

let () =
  init ();
  let spacing = 100 in 
  let circles = grid_of_circles spacing in
  show circles;
  close ();
  exit 0
