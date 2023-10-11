open Joy.Shape

let spacing = 50
let center = 250

let grid_of_circles () = 
  (* cartesian product - creates list of all combinations of the elements in two lists *)
  let cartesian_product l l' = 
    List.concat (List.map (fun e -> List.map (fun e' -> (e, e')) l') l) in
  let coords = cartesian_product [-1; 1] [1; -1] in 
  List.map (fun (x, y) -> circle ~x: (center + x * spacing) ~y: (center + y * spacing) 50) coords

let () =
  init ();
  let circles = grid_of_circles () in
  show circles;
  close ();
  exit 0
