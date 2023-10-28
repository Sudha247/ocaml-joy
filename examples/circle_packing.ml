open Joy.Shape

(* global constants // RNG initialization *)
let resolution = (500, 500)
let min_radius = 3
let max_radius = 50
let num_circles = 500
let max_attempts = 2000
let _ = Stdlib.Random.self_init

let palette =
  [
    (107, 108, 163);
    (135, 188, 189);
    (111, 153, 84);
    (150, 155, 199);
    (137, 171, 124);
    (67, 68, 117);
    (44, 45, 84);
  ]

(* utility Functions *)
let distance ({ x = x1; y = y1 } : point) ({ x = x2; y = y2 } : point) =
  let dx = float_of_int x2 -. float_of_int x1 in
  let dy = float_of_int y2 -. float_of_int y1 in
  let dist = sqrt ((dx *. dx) +. (dy *. dy)) in
  int_of_float dist

(* determines if two circles overlap *)
let overlap ({ c = c1; radius = r1 } : circle)
    ({ c = c2; radius = r2 } : circle) =
  let dist = distance c1 c2 in
  dist < r1 + r2

(* creates a random point within screen bounds *)
let rand_point () =
  {
    x = Stdlib.Random.full_int (fst resolution) - (fst resolution / 2);
    y = Stdlib.Random.full_int (snd resolution) - (snd resolution / 2);
  }

(* creates a circle with a random center point and radius *)
let rand_circle () =
  let point = rand_point () in
  {
    c = point;
    radius = min_radius + Stdlib.Random.full_int (max_radius - min_radius);
  }

let pack_circles () =
  (* checks whether a circle intersects with a list of circles *)
  let check_overlap lst current =
    List.fold_right (fun curr acc -> overlap curr current || acc) lst false
  in
  (* creates a new circle, checks if it intersects previous circles,
     if max attempts have been reached,
     or if the desired number of circles have been created.
     From there it either recurses with or without the new circle,
     or returns the list of circles *)
  let rec pack lst attempts =
    let new_circle = rand_circle () in
    let does_overlap = check_overlap lst new_circle in
    let safe = List.length lst < num_circles - 1 && attempts < max_attempts in
    if does_overlap && safe then pack lst (attempts + 1)
    else if not safe then new_circle :: lst
    else pack (new_circle :: lst) attempts
  in
  let attempts = 0 in
  let lst = [ { c = { x = 0; y = 0 }; radius = 10 } ] in
  pack lst attempts

(* pulls a random color from the 'palette' list
   sets draw color with it
   then draws circle *)
let draw_with_color circle =
  let idx = Stdlib.Random.full_int (List.length palette - 1) in
  let r, g, b = List.nth palette idx in
  Graphics.set_color (Graphics.rgb r g b);
  render_shape circle

let () =
  set_dimensions (fst resolution) (snd resolution);
  init ();
  Graphics.set_line_width 2;
  let circles = pack_circles () in
  (* converting circle type to more general 'shape' type for rendering *)
  let circles = List.map (fun c -> circle ~x:c.c.x ~y:c.c.y c.radius) circles in
  List.iter draw_with_color circles;
  close ()
