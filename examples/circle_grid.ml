open Graphics

type point = { x : int; y : int }
type circle = { c : point; radius : int }

type shape = Circle of circle

let canvas_size = (1000, 1000)
let canvas_mid = { x = fst canvas_size / 2; y = snd canvas_size / 2 }

let render_shape s =
  match s with
  | Circle circle -> Graphics.draw_circle circle.c.x circle.c.y circle.radius

let grid_of_circles ?x ?y spacing =
  let center = match (x, y) with
    | Some x, Some y -> { x; y }
    | _ -> canvas_mid in
  let create_circle x y =
    Circle { c = { x; y }; radius = 50 } 
  in
  let circles = ref [] in
  let half_spacing = spacing / 2 in
  let x_positions = [center.x - half_spacing; center.x + half_spacing] in
  let y_positions = [center.y - half_spacing; center.y + half_spacing] in
  for x = 0 to 1 do
    for y = 0 to 1 do
      let circle_x = List.nth x_positions x in
      let circle_y = List.nth y_positions y in
      circles := create_circle circle_x circle_y :: !circles
    done
  done;
  !circles

let show shapes = List.iter render_shape shapes

let () =
  Graphics.open_graph (" " ^ string_of_int (fst canvas_size) ^ "x" ^ string_of_int (snd canvas_size));
  set_color Graphics.black;

  let spacing = 100 in 
  let circles = grid_of_circles spacing in
  show circles;

  ignore (read_line ());
  Graphics.close_graph ()
