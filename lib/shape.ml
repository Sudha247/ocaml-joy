open Graphics

type point = { x : int; y : int }
type rectangle = { c : point; length : int; width : int }
type circle = { c : point; radius : int }
type ellipse = {c : point; rx : int; ry: int}
type shape = Circle of circle | Rectangle of rectangle | Ellipse of ellipse
type shapes = shape list

let dimensions = ref {x = 500; y = 500}
let set_dimensions x y = 
  dimensions := {x;y} 

let canvas_mid = { x = (!dimensions.x / 2); y = (!dimensions.y / 2)}

let axes_flag = ref false
let draw_axes flag = 
  axes_flag := flag

let render_axes () = 
  set_color (rgb 192 192 192);
  moveto (size_x () / 2) 0;
  lineto (size_x () / 2) (size_y ());
  moveto 0 (size_y () / 2);
  lineto (size_x ()) (size_y () / 2)

let render_shape s =
  match s with
  | Circle circle -> draw_circle circle.c.x circle.c.y circle.radius
  | Rectangle rectangle ->
      draw_rect rectangle.c.x rectangle.c.y rectangle.length rectangle.width
  | Ellipse ellipse ->
    draw_ellipse ellipse.c.x ellipse.c.y ellipse.rx ellipse.ry

let circle ?x ?y r =
  match (x, y) with
  | Some x, Some y -> Circle { c = { x; y }; radius = r }
  | _ -> Circle { c = { x = canvas_mid.x; y = canvas_mid.y }; radius = r }

let rectangle ?x ?y length width =
  match (x, y) with
  | Some x, Some y -> Rectangle { c = { x; y }; length; width }
  | _ -> Rectangle { c = { x = canvas_mid.x; y = canvas_mid.y }; length; width }

let ellipse ?x ?y rx ry =
  match (x, y) with
  | Some x, Some y -> Ellipse {c = {x; y}; rx; ry}
  | _ -> Ellipse {c = { x = canvas_mid.x; y = canvas_mid.y}; rx; ry}

let scale factor s =
  match s with
  | Circle circle' -> circle ~x:circle'.c.x ~y:circle'.c.y (circle'.radius*factor)
  | Rectangle rectangle' -> rectangle ~x:rectangle'.c.x ~y:rectangle'.c.y (rectangle'.length*factor) (rectangle'.width*factor)
  | Ellipse ellipse' -> ellipse ~x:ellipse'.c.x ~y:ellipse'.c.y (ellipse'.rx*factor) (ellipse'.ry*factor)

let show shapes = List.iter render_shape shapes

let init () =
  open_graph (Printf.sprintf " %ix%i" !dimensions.x !dimensions.y);
  if !axes_flag then
    render_axes ();
    
  set_color black

let close () =
  ignore (read_line ());
  close_graph ()
