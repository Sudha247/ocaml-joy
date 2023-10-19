open Joy.Shape 

(* 
  Complex shapes can also be created from lists of shapes.
  This allows us to apply any of the libraries' transformations to a group of 
  shapes, liek we're doing here with scale
*)

let rec range a b = if a > b then [] else a :: range (a + 1) b

let () = 
  init ();
  let comp = List.map (fun i -> circle ~x:(int_of_float ((float_of_int i /. 4.0) *. 100.0)) ~y: 0 50) (range (-5) 5) in 
  render_shape (scale 0.5 (complex comp));
  close ()