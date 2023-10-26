open Joy.Shape

let rec range a b = if a > b then [] else a :: range (a +. 1.) b
let num_points = 16.
let size = 100

let idx_to_point i =
  let x = cos (2. *. Float.pi *. i /. num_points) in
  let y = sin (2. *. Float.pi *. i /. num_points) in
  {
    x = int_of_float (x *. float_of_int size);
    y = int_of_float (y *. float_of_int size);
  }

let () =
  init ();
  let points = List.map idx_to_point (range 0. num_points) in
  let _rect =
    [
      { x = 0; y = 0 };
      { x = 0; y = size };
      { x = size; y = size };
      { x = size; y = 0 };
    ]
  in
  let poly = polygon points in
  render_shape poly;
  close ()
