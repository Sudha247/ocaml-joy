open Joy.Shape

let row_of_circles dim_x dim_y spacing =
  let circles = ref [] in
  let total_width = 3 * spacing in 
  let (center_x, center_y) = (dim_x / 2, dim_y / 2) in
  let start_x = center_x - total_width / 2 in 
  for i = 0 to 2 do
    let x = start_x + i * spacing in
    circles := circle ~x:x ~y:center_y 50 :: !circles
  done;
  !circles

let () =
  init ();
  let (dim_x, dim_y) = (1000,1000) in
  set_dimensions dim_x dim_y;
  let spacing = 120 in 
  let circles = row_of_circles dim_x dim_y spacing in
  show circles;
  ignore (read_line ());
  close()
