open Graphics

type point = { x : int; y : int }
type circle = { c : point; radius : int }
type rectangle = { c : point; length : int; width : int }

type shape = Circle of circle | Rectangle of rectangle

let canvas_size = (500, 500)
let canvas_mid = { x = fst canvas_size / 2; y = snd canvas_size / 2 }

let render_shape s =
  match s with
  | Circle circle -> Graphics.draw_circle circle.c.x circle.c.y circle.radius
  | Rectangle rect ->
      let x1 = rect.c.x - (rect.length / 2) in
      let x2 = rect.c.x + (rect.length / 2) in
      let y1 = rect.c.y - (rect.width / 2) in
      let y2 = rect.c.y + (rect.width / 2) in
      Graphics.draw_rect x1 y1 (x2 - x1) (y2 - y1)

let circle radius =
  let center = canvas_mid in
  Circle { c = center; radius }

let rectangle_outside_circle rectangle_length rectangle_width circle_radius =
  let circle_center = canvas_mid in
  let circle_cx = circle_center.x in
  let circle_cy = circle_center.y in
  let rectangle_cx = circle_cx in
  let rectangle_cy = circle_cy in
  let distance_x = circle_radius + (rectangle_length / 2) + 20 in
  let distance_y = circle_radius + (rectangle_width / 2) + 20 in
  Rectangle { c = { x = rectangle_cx; y = rectangle_cy }; length = distance_x; width = distance_y }

let show shapes = List.iter render_shape shapes

let () =
  Graphics.open_graph (" " ^ string_of_int (fst canvas_size) ^ "x" ^ string_of_int (snd canvas_size));
  set_color Graphics.black;

  let circle_shape = circle 40 in 
  let rectangle_shape = rectangle_outside_circle 120 80 60 in 

  show [circle_shape; rectangle_shape];

  ignore (read_line ());
  Graphics.close_graph ()
