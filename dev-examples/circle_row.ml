open Graphics

type point = { x : int; y : int }
type circle = { c : point; radius : int }
type shape = Circle of circle

let canvas_size = (1000, 1000)
let canvas_mid = { x = fst canvas_size / 2; y = snd canvas_size / 2 }

let render_shape s =
  match s with
  | Circle circle -> draw_circle circle.c.x circle.c.y circle.radius

let row_of_circles ?x ?y spacing =
  let center =
    match (x, y) with Some x, Some y -> { x; y } | _ -> canvas_mid
  in
  let create_circle x = Circle { c = { x; y = center.y }; radius = 50 } in
  let circles = ref [] in
  let total_width = 3 * spacing in
  let start_x = center.x - (total_width / 2) in
  for i = 0 to 2 do
    let x = start_x + (i * spacing) in
    circles := create_circle x :: !circles
  done;
  !circles

let show shapes = List.iter render_shape shapes

let () =
  open_graph
    (" "
    ^ string_of_int (fst canvas_size)
    ^ "x"
    ^ string_of_int (snd canvas_size));
  set_color black;

  let spacing = 150 in
  let circles = row_of_circles spacing in
  show circles;

  ignore (read_line ());
  close_graph ()
