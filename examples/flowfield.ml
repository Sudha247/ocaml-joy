open Joy

(* Constants *)
let size = 1200
let tau = 2. *. Float.pi
let num_steps = 12
let grid_divisor = 256
let _ = Random.self_init ()
let octaves = 4
let noise_scale = 2. +. Random.float 3.

(* Utilities & color palette *)

let shuffle xs =
  let nd = List.map (fun c -> (Random.bits (), c)) xs in
  let sond = List.sort compare nd in
  List.map snd sond

let palette =
  List.map
    (fun (r, g, b) -> (r /. 255., g /. 255., b /. 255.))
    [
      (74., 58., 59.);
      (152., 65., 54.);
      (194., 106., 122.);
      (236., 192., 161.);
      (240., 240., 228.);
    ]
  |> shuffle

let clamp = function
  | n when n > size - 1 -> size - 1
  | n when n < 0 -> 0
  | n -> n

let fclamp max = function f when f > max -> max | f when f < 0. -> 0. | f -> f

(* Initialize flowfield, a large 2D array containing angles determined by
   seeded simplex noise sampled at each coordinate *)
let flowfield () =
  let seed = Random.float 100. in
  Bigarray.Array2.init Bigarray.Float32 Bigarray.c_layout size size (fun x y ->
      let noise =
        Noise.fractal2 octaves
          ((float_of_int x /. float_of_int size *. noise_scale) +. seed)
          ((float_of_int y /. float_of_int size *. noise_scale) +. seed)
      in
      let uni = (noise *. 0.5) +. 0.5 in
      fclamp tau uni *. tau)

(* Create a n*n grid of points where lines will be placed *)
let grid divison =
  let grid_size = size / divison in
  let spacing = size / grid_size in
  Array.init (grid_size * grid_size) (fun i ->
      (i / grid_size * spacing, i mod grid_size * spacing))

(* scale 0-n coordinates to [-n/2..n/2] *)
let uni_to_bi (x, y) =
  let x = x - (size / 2) in
  let y = y - (size / 2) in
  (float_of_int x, float_of_int y)

(* Create a 2D vector from an angle *)
let vector_of_angle angle =
  ( sin angle |> Float.round |> int_of_float,
    cos angle |> Float.round |> int_of_float )

(* Step along the flowfield, following the angles at each point visited *)
let rec step n (x, y) flowfield =
  if n >= 0 then
    let cx, cy = (clamp x, clamp y) in
    let angle = Bigarray.Array2.get flowfield cx cy in
    let dx, dy = vector_of_angle angle in
    step (n - 1) (x + dx, y + dy) flowfield
  else (x, y)

(* Given a coordinate, draws a line starting at that point, following flowfield *)
let make_line flowfield (x, y) =
  let cx, cy = (clamp x, clamp y) in
  let angle = Bigarray.Array2.get flowfield cx cy in
  let dx, dy = vector_of_angle angle in
  let next = (x + dx, y + dy) in
  let final = step num_steps next flowfield in
  let ax, ay = uni_to_bi (x, y) in
  let bx, by = uni_to_bi final in
  (line ~a:(point ax ay) (point bx by), (cx, cy))

(* Renders line with color based on its angle *)
let render_with_color flowfield line (x, y) =
  let color_idx =
    Bigarray.Array2.get flowfield x y /. tau
    |> ( *. ) (float_of_int (List.length palette))
    |> int_of_float
  in
  let color = List.nth palette color_idx in
  set_color color;
  render line

let () =
  init ();
  background (1., 1., 1., 1.);
  set_line_width 0.0005;
  let flowfield = flowfield () in
  let interval = size / grid_divisor in
  let indices = grid interval in
  let lines, points = Array.map (make_line flowfield) indices |> Array.split in
  let centered =
    Array.map (translate (float_of_int interval) (float_of_int interval)) lines
  in
  Array.iter2 (render_with_color flowfield) centered points;
  write ~filename:"flowfield.png" ()
