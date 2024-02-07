open Joy

(* global constants // RNG initialization *)
let w, h = (1200., 900.)
let min_radius = 20.
let max_radius = 150.
let num_circles = 5_000
let max_attempts = 100_000
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

(* distance between two points *)
let distance (x1, y1) (x2, y2) =
  let dx = x2 -. x1 in
  let dy = y2 -. y1 in
  sqrt ((dx *. dx) +. (dy *. dy))

(* determines if two circles overlap *)
let overlaps (p1, r1) (p2, r2) =
  let dist = distance p1 p2 in
  dist < r1 +. r2

(* creates a random point within screen bounds *)
let rand_point () =
  (Random.float w -. (w /. 2.), Random.float h -. (h /. 2.))

(* creates a circle with a random center point and radius *)
let rand_circle () =
  let point = rand_point () in
  (point, min_radius +. Random.float (max_radius -. min_radius))

(* creates a lis of packed circles *)
let pack_circles () =
  (* checks whether a circle intersects with a list of circles *)
  let overlap_any lst current =
    List.fold_right (fun circle acc -> overlaps circle current || acc) lst false
  in
  let rec find_new old attempts = 
    let new_circle = rand_circle () in 
    if overlap_any old new_circle then 
      find_new old (attempts + 1)
    else
      (new_circle :: old, attempts)
  in
  let rec pack lst = function 
    | n when n < max_attempts && List.length lst <= num_circles -> 
      let circles, n = find_new lst n in 
      pack circles (n + 1)
    | _ -> 
      lst
  in 
  pack [] 0

(* Creates a Joy circle from the tuple representation used in packing *)
let circle_from_repr ((x, y), r) = circle ~c:{x; y} (int_of_float r)

(* pulls a random color from the 'palette' list
   sets draw color with it
   then draws circle *)
let draw_with_color circle =
  let idx = Random.full_int (List.length palette - 1) in
  set_color (List.nth palette idx);
  render circle

(* main fn *)
let () =
  init ~size:(int_of_float w, int_of_float h) ();
  background (255, 255, 255, 255);
  set_line_width 1;
  let circles = pack_circles () |> List.map circle_from_repr in
  let circles = List.map (repeat 8 (scale 0.9)) circles in 
  List.iter draw_with_color circles;
  write ~filename:"Circle packing.png" ()
