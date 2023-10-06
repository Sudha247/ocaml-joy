open Graphics

type point = { x : int; y : int }
type rectangle = { c : point; length : int; width : int }
type circle = { c : point; radius : int }
type ellipse = { c : point; rx : int; ry : int }
type triangle = { p1 : point; p2 : point; p3 : point }
type shape = Circle of circle | Rectangle of rectangle | Ellipse of ellipse | Triangle of triangle
type shapes = shape list

let canvas_mid = { x = 250; y = 250 }

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
  | Triangle triangle ->
      draw_poly [| triangle.p1.x, triangle.p1.y; triangle.p2.x, triangle.p2.y; triangle.p3.x, triangle.p3.y |]

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
  | Some x, Some y -> Ellipse { c = { x; y }; rx; ry }
  | _ -> Ellipse { c = { x = canvas_mid.x; y = canvas_mid.y }; rx; ry }

let triangle ?x1 ?y1 ?x2 ?y2 ?x3 ?y3 () =
  let p1 =
    match (x1, y1) with
    | Some x, Some y -> { x; y }
    | _ -> canvas_mid
  in
  let p2 =
    match (x2, y2) with
    | Some x, Some y -> { x; y }
    | _ -> canvas_mid
  in
  let p3 =
    match (x3, y3) with
    | Some x, Some y -> { x; y }
    | _ -> canvas_mid
  in
  Triangle { p1; p2; p3 }

let translate dx dy shape =
  match shape with
  | Circle circle -> Circle { circle with c = { x = circle.c.x + dx; y = circle.c.y + dy } }
  | Rectangle rectangle -> Rectangle { rectangle with c = { x = rectangle.c.x + dx; y = rectangle.c.y + dy } }
  | Ellipse ellipse -> Ellipse { ellipse with c = { x = ellipse.c.x + dx; y = ellipse.c.y + dy } }
  | Triangle triangle ->
      Triangle { p1 = { x = triangle.p1.x + dx; y = triangle.p1.y + dy };
                 p2 = { x = triangle.p2.x + dx; y = triangle.p2.y + dy };
                 p3 = { x = triangle.p3.x + dx; y = triangle.p3.y + dy } }

let show shapes = List.iter render_shape shapes

let init () =
  open_graph " 500x500";
  if !axes_flag then
    render_axes ();
    
  set_color black

let close () =
  ignore (read_line ());
  close_graph ()