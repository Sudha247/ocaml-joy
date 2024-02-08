open Joy

(* global constants // RNG initialization *)
let size = (800, 800)
let min_radius = 20.
let max_radius = 150.
(* Maximum number of circles *)
let num_circles = 5_000
(* Maximum attempts at finding new non-overlapping circles *)
let max_attempts = 100_000
(* Factor circles are scaled by *)
let shrink_factor = 0.85
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
let ( >> ) f g x = g (f x)

(* distance between two points *)
let distance (x1, y1) (x2, y2) =
  let dx = x2 -. x1 in
  let dy = y2 -. y1 in
  sqrt ((dx *. dx) +. (dy *. dy))

(* determines if two circles overlap *)
let overlaps (p1, r1) (p2, r2) =
  let dist = distance p1 p2 in
  dist < r1 +. r2

(* Creates a random point within screen bounds *)
let rand_point () =
  let w, h = tmap float_of_int size in
  (Random.float w -. (w /. 2.), Random.float h -. (h /. 2.))

(* Creates a circle with a random center point and radius *)
let rand_circle () =
  let point = rand_point () in
  (point, min_radius +. Random.float (max_radius -. min_radius))

(* Creates a list of packed(non-ovelapping) circles *)
let pack_circles () =
  (* checks whether a circle intersects with a list of circles *)
  let overlap_any lst current =
    List.fold_right (fun circle acc -> overlaps circle current || acc) lst false
  in
  (* Atttempts to create a circle that meets all the necessary conditions, recurses*)
  let rec pack lst = function 
    | n when n < max_attempts && List.length lst <= num_circles -> 
      let new_circle = rand_circle () in 
      if overlap_any lst new_circle then 
        pack lst (n + 1)
      else
        pack (new_circle :: lst) n
    | _ -> lst
  in 
  pack [] 0

let shrink c = 
  let rec shrink' = function 
    | ((pos, r) :: _) as lst when r > 1. -> 
      shrink' ((pos, r *. shrink_factor) :: lst)
    | lst -> lst
  in
  shrink' [ c ]

(* Creates a Joy circle from the tuple representation used in the packing fn *)
let circle_from_repr ((x, y), r) = circle ~c:{x; y} (int_of_float r)

(* Pulls a random color from the 'palette' list
   sets stroke color with it, and
   then renders circle *)
let draw_with_color circle =
  let idx = Random.full_int (List.length palette - 1) in
  set_color (List.nth palette idx);
  render circle

(* Main *)
let () =
  (* Initialize canvas with `size` resolution *)
  init ~size ();
  (* Set an opaque white background *)
  background (255, 255, 255, 255);
  (* Create packed circles *)
  let circles = pack_circles () in
  (* Make them concentric (each becomes a list of circles), then 
     convert them to Joy representation from tuple representation *)
  let circles = List.map (shrink >> List.map circle_from_repr) circles in
  (* Convert to a flat list of circles *)
  let circles = List.flatten circles in
  (* Render them! *)
  List.iter draw_with_color circles;
  (* Write to PNG *)
  write ~filename:"Circle-packing.png" ()
