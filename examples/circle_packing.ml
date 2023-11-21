open Joy.Shape

(* global constants // RNG initialization *)
let resolution = (1200, 900)
let min_radius = 20
let max_radius = 150
let num_circles = 5_000
let max_attempts = 100_000
let shrink_factor = 0.85
let _ = Stdlib.Random.self_init ()

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
  let dx = float_of_int x2 -. float_of_int x1 in
  let dy = float_of_int y2 -. float_of_int y1 in
  let dist = sqrt ((dx *. dx) +. (dy *. dy)) in
  int_of_float dist

(* determines if two circles overlaps *)
let overlaps (p1, r1) (p2, r2) =
  let dist = distance p1 p2 in
  dist < r1 + r2

(* creates a random point within screen bounds *)
let rand_point () =
  ( Stdlib.Random.full_int (fst resolution * 2) - fst resolution,
    Stdlib.Random.full_int (snd resolution * 2) - snd resolution )

(* creates a circle with a random center point and radius *)
let rand_circle () =
  let point = rand_point () in
  (point, min_radius + Stdlib.Random.full_int (max_radius - min_radius))

(* creates a lis of packed circles *)
let pack_circles () =
  (* checks whether a circle intersects with a list of circles *)
  let check_overlaps lst current =
    List.fold_right (fun curr acc -> overlaps curr current || acc) lst false
  in
  (* creates a new circle, checks if it intersects previous circles,
     if max attempts have been reached,
     or if the desired number of circles have been created.
     From there it either recurses with or without the new circle,
     or returns the list of circles *)
  let rec pack lst attempts =
    let new_circle = rand_circle () in
    let does_overlap = check_overlaps lst new_circle in
    let safe = List.length lst < num_circles - 1 && attempts < max_attempts in
    if does_overlap && safe then pack lst (attempts + 1)
    else if not safe then new_circle :: lst
    else pack (new_circle :: lst) attempts
  in
  let attempts = 0 in
  let lst = [ rand_circle () ] in
  pack lst attempts

(* pulls a random color from the 'palette' list
   sets draw color with it
   then draws circle *)
let draw_with_color circle =
  let idx = Stdlib.Random.full_int (List.length palette - 1) in
  let r, g, b = List.nth palette idx in
  Graphics.set_color (Graphics.rgb r g b);
  render_shape circle

(* turns a circle into a list of concentric circles *)
let make_concentric circle =
  let rec shrink lst =
    let point, radius = List.hd (List.rev lst) in
    if radius <= 1 then lst
    else
      let new_circle =
        (point, int_of_float (float_of_int radius *. shrink_factor))
      in
      shrink (lst @ [ new_circle ])
  in
  shrink [ circle ]

(* main fn *)
let () =
  set_dimensions (fst resolution) (snd resolution);
  init ();
  Graphics.set_line_width 3;
  let circles = pack_circles () in
  let circles = List.flatten (List.map make_concentric circles) in
  List.iter
    (fun ((x, y), radius) -> draw_with_color (circle ~x ~y radius))
    circles;
  close ()
