open Joy.Svg

(*
   Complex shapes can also be created from lists of shapes.
   This allows us to apply any of the libraries' transformations to a group of
   shapes, like we're doing here with translate
*)

(* creates a list containing numbers between a and b *)
let rec range a b = if a > b then [] else a :: range (a + 1) b

(* creates a list of (int * int) tuples,
   containing every combination of the elements of the two passsed lists *)
let cartesian_product l l' =
  List.concat (List.map (fun e -> List.map (fun e' -> (e, e')) l') l)

let () =
  init ();
  background (255, 255, 255, 255);
  (* radius which also acts as grid spacing *)
  let radius = 50 in
  let half_radius = radius / 2 in
  (* creating a grid with cartesian_product *)
  let coords = cartesian_product (range (-5) 5) (range (-5) 5) in
  (* using map to turn that into a complex shape that is a grid of circles *)
  let complex_shape =
    complex
      (List.map
         (fun (x, y) -> circle ~c:(point (x * radius) (y * radius)) radius)
         coords)
  in
  (* translating that complex shape by radius / 2 *)
  let complex_transformed = translate half_radius half_radius complex_shape in
  set_color (0, 0, 0);
  show [ complex_shape; complex_transformed ];
  write ~filename:"complex.png" ()
