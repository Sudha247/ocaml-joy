open Joy

(* global constants // RNG initialization *)
let w, h = (900., 900.)
let min_radius = 20.
let max_radius = 150.
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
  let dx = x2 -. x1 in
  let dy = y2 -. y1 in
  sqrt ((dx *. dx) +. (dy *. dy))

(* creates a random point within screen bounds *)
let rand_point () =
  (Stdlib.Random.float w -. (w /. 2.), Stdlib.Random.float h -. (h /. 2.))

(* creates a circle with a random center point and radius *)
let rand_circle () =
  let point = rand_point () in
  (point, min_radius +. Stdlib.Random.float (max_radius -. min_radius))

(* creates a lis of packed circles *)
let pack_circles () =
  (* determines if two circles overlap *)
  let overlaps (p1, r1) (p2, r2) =
    let dist = distance p1 p2 in
    dist < r1 +. r2
  in
  (* checks whether a circle intersects with a list of circles *)
  let check_overlaps lst current =
    List.fold_right (fun circle acc -> overlaps circle current || acc) lst false
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
    match (does_overlap, safe) with
    | true, true -> pack lst (attempts + 1)
    | true, false -> new_circle :: lst
    | _ -> pack (new_circle :: lst) attempts
  in
  let attempts = 0 in
  let lst = [ rand_circle () ] in
  pack lst attempts

(* pulls a random color from the 'palette' list
   sets draw color with it
   then draws circle *)
let draw_with_color circle =
  let idx = Stdlib.Random.full_int (List.length palette - 1) in
  set_color (List.nth palette idx);
  render circle

(* turns a circle into a list of concentric circles *)
let make_concentric circle =
  let rec shrink lst =
    let point, radius = List.hd (List.rev lst) in
    if radius <= 1. then lst
    else
      let new_circle = (point, radius *. shrink_factor) in
      shrink (lst @ [ new_circle ])
  in
  shrink [ circle ]

(* main fn *)
let () =
  init ~size:(int_of_float w, int_of_float h) ();
  background (255, 255, 255, 255);
  set_line_width 1;
  let circles = pack_circles () in
  let circles = List.flatten (List.map make_concentric circles) in
  List.iter
    (fun ((x, y), radius) ->
      draw_with_color
        (circle
           ~c:(point (int_of_float x) (int_of_float y))
           (int_of_float radius)))
    circles;
  write ~filename:"Circle packing.png" ()
