open Joy

(* global constants // RNG initialization *)
let size = (800, 800)
let min_radius = 5.
let max_radius = 150.
let pareto_scale = 1.
let pareto_alpha = 0.25

(* Maximum number of circles *)
let num_circles = 10000

(* Maximum attempts at finding new non-overlapping circles *)
let max_attempts = 100_000

(* Factor circles are scaled by *)
let _ = Random.self_init ()

let palette =
  [
    (* purple *)
    (107, 108, 163);
    (* light blue *)
    (135, 188, 189);
    (* green *)
    (111, 153, 84);
    (* light purple *)
    (150, 155, 199);
    (* light green *)
    (137, 171, 124);
    (* dark purple *)
    (67, 68, 117);
    (* darker purple *)
    (44, 45, 84);
  ]

(* utility Functions *)
let tmap f (a, b) = (f a, f b)
let rand_nth coll = List.length coll |> Random.full_int |> List.nth coll

(* Pareto distribution float random for radii *)
let pareto scl alpha =
  let x = Random.float (1. -. Float.epsilon) +. Float.epsilon in
  scl *. ((1. -. x) ** (-1. /. alpha))

(* Clamp floats to [min..max] *)
let clamp min max = function
  | n when n >= max -> max
  | n when n <= min -> min
  | n -> n

(* distance between two points *)
let distance (x1, y1) (x2, y2) =
  let dx = x2 -. x1 in
  let dy = y2 -. y1 in
  sqrt ((dx *. dx) +. (dy *. dy))

(* determines if two circles overlap *)
let overlaps (x1, y1, r1) (x2, y2, r2) =
  let dist = distance (x1, y1) (x2, y2) in
  dist < r1 +. r2

(* Creates a random point within screen bounds *)
let rand_point () =
  let w, h = tmap float_of_int size in
  (Random.float w -. (w /. 2.), Random.float h -. (h /. 2.))

(* Creates a circle with a random center point and radius *)
let rand_circle () =
  let x, y = rand_point () in
  (x, y, pareto pareto_scale pareto_alpha |> clamp min_radius max_radius)

(* Creates a list of packed(non-ovelapping) circles *)
let pack_circles () =
  (* checks whether a circle intersects with a list of circles *)
  let rec overlap_any current = function
    | [] -> false
    | x :: xs -> overlaps current x || overlap_any current xs
  in
  (* Atttempts to create a circle that meets all the necessary conditions,
     recurses with list of circles *)
  let rec pack n lst =
    if n >= max_attempts || List.length lst >= num_circles then lst
    else
      let new_circle = rand_circle () in
      if overlap_any new_circle lst then pack (n + 1) lst
      else pack (n + 1) (new_circle :: lst)
  in
  pack 0 []

(* Creates a Joy circle from the tuple representation used in the packing fn *)
let circle_from_repr (x, y, r) = circle ~c:{ x; y } (int_of_float r)

(* Main *)
let () =
  (* Initialize canvas with `size` resolution *)
  init ~size ();
  (* Create packed circles *)
  pack_circles ()
  (* Convert from our tuple representation to the Joy representation *)
  |> List.map circle_from_repr
  (* Add a stroke color *)
  |> List.map (fun c -> with_fill (rand_nth palette) c)
  (* Render them! *)
  |> show;
  (* Write to PNG *)
  write ~filename:"circle-packing.png" ()
