(* Constants *)
let size = 1200
let tau = 2. *. Float.pi
let num_steps = 6
let grid_divisor = 128
let octaves = 4
let noise_scale = 2. +. Joy.frandom 3.

(* Utilities & color palette *)

(* Randomly shuffles a list *)
let shuffle xs =
  let pairs = List.map (fun c -> (Joy.random 0xFFFF, c)) xs in
  let sorted = List.sort compare pairs in
  List.map snd sorted

let palette =
  [
    (74, 58, 59);
    (152, 65, 54);
    (194, 106, 122);
    (236, 192, 161);
    (240, 240, 228);
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
  let seed = Joy.frandom 100. in
  Bigarray.Array2.init Bigarray.Float32 Bigarray.c_layout size size (fun x y ->
      let noise =
        Joy.fractal_noise ~octaves
          [
            (float_of_int x /. float_of_int size *. noise_scale) +. seed;
            (float_of_int y /. float_of_int size *. noise_scale) +. seed;
          ]
      in
      let uni = (noise *. 0.5) +. 0.5 in
      fclamp tau uni *. tau)

(* Create a n*n grid of points where lines will be placed *)
let grid divison =
  let grid_size = size / divison in
  let spacing = size / grid_size in
  List.init (grid_size * grid_size) (fun i ->
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
  (Joy.line ~a:{ x = ax; y = ay } { x = bx; y = by }, (cx, cy))

(* Adds color to line, based on its angle *)
let add_color flowfield line (x, y) =
  let color =
    Bigarray.Array2.get flowfield x y /. tau
    |> ( *. ) (float_of_int (List.length palette))
    |> int_of_float |> List.nth palette
  in
  line |> Joy.with_stroke color

let () =
  let open Joy in
  init ();
  set_line_width 3;
  let flowfield = flowfield () in
  let interval = size / grid_divisor in
  let indices = grid interval in
  let lines, points = List.map (make_line flowfield) indices |> List.split in
  let centered = List.map (translate interval interval) lines in
  let lines = List.map2 (add_color flowfield) centered points in
  show lines;
  write ~filename:"flowfield.png" ()
