open Graphics

type point = { x : int; y : int }
type rectangle = { c : point; length : int; width : int }
type shape = Rectangle of rectangle

let canvas_size = (500, 500)
let canvas_mid = { x = fst canvas_size / 2; y = snd canvas_size / 2 }

let render_shape s =
  match s with
  | Rectangle rect ->
      let x1 = rect.c.x - (rect.length / 2) in
      let x2 = rect.c.x + (rect.length / 2) in
      let y1 = rect.c.y - (rect.width / 2) in
      let y2 = rect.c.y + (rect.width / 2) in
      draw_rect x1 y1 (x2 - x1) (y2 - y1)

let rectangle ?x ?y length width =
  let center =
    match (x, y) with Some x, Some y -> { x; y } | _ -> canvas_mid
  in
  Rectangle { c = center; length; width }

let show shapes = List.iter render_shape shapes

let () =
  open_graph
    (" "
    ^ string_of_int (fst canvas_size)
    ^ "x"
    ^ string_of_int (snd canvas_size));
  set_color black;

  let rect = rectangle 200 100 in
  show [ rect ];

  ignore (read_line ());
  close_graph ()
