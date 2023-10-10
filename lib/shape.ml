open Graphics

type point = { x : int; y : int }
type rectangle = { c : point; length : int; width : int }
type circle = { c : point; radius : int }
type ellipse = { c : point; rx : int; ry: int }
type shape = Circle of circle | Rectangle of rectangle | Ellipse of ellipse
type shapes = shape list

let dimensions = ref {x = 500; y = 500}
let set_dimensions x y = 
  dimensions := {x; y}

let canvas_center = ref {x = 0; y = 0}

let axes_flag = ref false
let draw_axes flag = 
  axes_flag := flag

let render_axes () = 
  set_color (rgb 192 192 192);
  moveto (!dimensions.x / 2) 0;
  lineto (!dimensions.x / 2) (!dimensions.y);
  moveto 0 (!dimensions.y / 2);
  lineto (!dimensions.x) (!dimensions.y / 2)

let render_shape s =
  match s with
  | Circle circle -> draw_circle (circle.c.x + !canvas_center.x) (-circle.c.y + !canvas_center.y) circle.radius
  | Rectangle rectangle ->
      let c = rectangle.c in
      draw_rect (c.x + !canvas_center.x - (rectangle.length / 2)) (-c.y + !canvas_center.y - (rectangle.width / 2)) rectangle.length rectangle.width
  | Ellipse ellipse ->
      let c = ellipse.c in
      draw_ellipse (c.x + !canvas_center.x) (-c.y + !canvas_center.y) ellipse.rx ellipse.ry

let circle ?x ?y r =
  match (x, y) with
  | Some x, Some y -> Circle { c = { x; y }; radius = r }
  | _ -> Circle { c = { x = 0; y = 0 }; radius = r }

let rectangle ?x ?y length width =
  match (x, y) with
  | Some x, Some y -> Rectangle { c = { x; y }; length; width }
  | _ -> Rectangle { c = { x = 0; y = 0 }; length; width }

let ellipse ?x ?y rx ry =
  match (x, y) with
  | Some x, Some y -> Ellipse { c = { x; y }; rx; ry }
  | _ -> Ellipse { c = { x = 0; y = 0 }; rx; ry }

let translate dx dy shape =
  match shape with
  | Circle circle -> Circle { circle with c = { x = circle.c.x + dx; y = circle.c.y + dy } }
  | Rectangle rectangle -> Rectangle { rectangle with c = { x = rectangle.c.x + dx; y = rectangle.c.y + dy } }
  | Ellipse ellipse -> Ellipse { ellipse with c = { x = ellipse.c.x + dx; y = ellipse.c.y + dy } }

let show shapes = List.iter render_shape shapes

let init () =
  open_graph (Printf.sprintf " %ix%i" !dimensions.x !dimensions.y);
  if !axes_flag then
    render_axes ();
    
  set_color black

let close () =
  ignore (read_line ());
  close_graph ()