open Graphics

type point = { x : int; y : int }
type circle = { c : point; radius : int }

type shape = Circle of circle

let canvas_size = (500, 500)
let canvas_mid = { x = fst canvas_size / 2; y = snd canvas_size / 2 }

let render_shape s =
  match s with
  | Circle circle -> draw_circle circle.c.x circle.c.y circle.radius

let concentric_circles ?x ?y num_circles spacing =
  let center = match (x, y) with
    | Some x, Some y -> { x; y }
    | _ -> canvas_mid in
  let rec create_circles n radius acc =
    if n <= 0 then acc
    else
      let c = Circle { c = center; radius } in
      create_circles (n - 1) (radius + spacing) (c :: acc)
  in
  create_circles num_circles 10 []

let show shapes = List.iter render_shape shapes

let () =
  open_graph (" " ^ string_of_int (fst canvas_size) ^ "x" ^ string_of_int (snd canvas_size));
  set_color black;

  let circles = concentric_circles 5 20 in
  show circles;

  ignore (read_line ());
  close_graph ()
