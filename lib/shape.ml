open Graphics

type point = { x : int; y : int }
type rectangle = { c : point; length : int; width : int }
type circle = { c : point; radius : int }
type ellipse = { c : point; rx : int; ry : int }
type shape = Circle of circle | Rectangle of rectangle | Ellipse of ellipse
type shapes = shape list

let dimensions = ref { x = 500; y = 500 }
let set_dimensions x y =
  dimensions := { x; y }

let canvas_center = ref { x = 0; y = 0 }

let axes_flag = ref false
let draw_axes flag =
  axes_flag := flag

let render_axes () =
  set_color (rgb 192 192 192);
  moveto (!dimensions.x / 2) 0;
  lineto (!dimensions.x / 2) (!dimensions.y);
  moveto 0 (!dimensions.y / 2);
  lineto (!dimensions.x) (!dimensions.y / 2)

let denormalize x y =
  { x = x + !canvas_center.x; y = -y + !canvas_center.y }

let render_shape s =
  match s with
  | Circle circle ->
      let c = denormalize circle.c.x circle.c.y in
      draw_circle c.x c.y circle.radius
  | Rectangle rectangle ->
      let c = denormalize rectangle.c.x rectangle.c.y in
      draw_rect (c.x - (rectangle.length / 2)) (c.y - (rectangle.width / 2)) rectangle.length rectangle.width
  | Ellipse ellipse ->
      let c = denormalize ellipse.c.x ellipse.c.y in
      draw_ellipse c.x c.y ellipse.rx ellipse.ry

let circle ?x ?y r =
  let default_center = { x = 0; y = 0 } in
  let center = match (x, y) with Some x, Some y -> { x; y } | _ -> default_center in
  Circle { c = center; radius = r }

let rectangle ?x ?y length width =
  let default_center = { x = 0; y = 0 } in
  let center = match (x, y) with Some x, Some y -> { x; y } | _ -> default_center in
  Rectangle { c = center; length; width }

let ellipse ?x ?y rx ry =
  let default_center = { x = 0; y = 0 } in
  let center = match (x, y) with Some x, Some y -> { x; y } | _ -> default_center in
  Ellipse { c = center; rx; ry }

let translate dx dy shape =
  match shape with
  | Circle circle ->
      let c = denormalize circle.c.x circle.c.y in
      Circle { circle with c = { x = c.x + dx; y = c.y + dy } }
  | Rectangle rectangle ->
      let c = denormalize rectangle.c.x rectangle.c.y in
      Rectangle { rectangle with c = { x = c.x + dx; y = c.y + dy } }
  | Ellipse ellipse ->
      let c = denormalize ellipse.c.x ellipse.c.y in
      Ellipse { ellipse with c = { x = c.x + dx; y = c.y + dy } }

let show shapes = List.iter (fun shape -> render_shape shape) shapes

let init () =
  open_graph (Printf.sprintf " %ix%i" !dimensions.x !dimensions.y);
  if !axes_flag then
    render_axes ();

  set_color black

let close () =
  ignore (read_line ());
  close_graph ()