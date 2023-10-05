open Graphics

type point = { x : int; y : int }
type rectangle = { c : point; length : int; width : int }
type circle = { c : point; radius : int }
type ellipse = { c : point; rx : int; ry : int }
type triangle = { p1 : point; p2 : point; p3 : point }
type star = { center : point; inner_radius : int; outer_radius : int; spikes : int }
type shape = Circle of circle | Rectangle of rectangle | Ellipse of ellipse | Triangle of triangle | Star of star
type shapes = shape list

let canvas_mid = { x = 250; y = 250 }

let render_shape s =
  match s with
  | Circle circle -> draw_circle circle.c.x circle.c.y circle.radius
  | Rectangle rectangle ->
      draw_rect rectangle.c.x rectangle.c.y rectangle.length rectangle.width
  | Ellipse ellipse ->
      draw_ellipse ellipse.c.x ellipse.c.y ellipse.rx ellipse.ry
  | Triangle triangle ->
      draw_poly [|triangle.p1.x, triangle.p1.y; triangle.p2.x, triangle.p2.y; triangle.p3.x, triangle.p3.y|]
  | Star star ->
      let draw_star x y inner outer spikes =
        let angle_step = 2.0 *. Float.pi /. float_of_int (spikes * 2) in
        let rec draw_lines x1 y1 angle = function
          | 0 -> ()
          | n ->
              let x2 = x1 + int_of_float (cos angle *. float_of_int outer) in
              let y2 = y1 + int_of_float (sin angle *. float_of_int outer) in
              moveto x1 y1;
              lineto x2 y2;
              let x3 = x1 + int_of_float (cos (angle +. angle_step) *. float_of_int inner) in
              let y3 = y1 + int_of_float (sin (angle +. angle_step) *. float_of_int inner) in
              moveto x1 y1;
              lineto x3 y3;
              draw_lines x1 y1 (angle +. 2.0 *. angle_step) (n - 1)
        in
        draw_lines x y 0.0 spikes
      in
      draw_star star.center.x star.center.y star.inner_radius star.outer_radius star.spikes

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

let star ?x ?y inner outer spikes =
  let center =
    match (x, y) with
    | Some x, Some y -> { x; y }
    | _ -> canvas_mid
  in
  Star { center; inner_radius = inner; outer_radius = outer; spikes }

let translate dx dy shape =
  match shape with
  | Circle circle -> Circle { circle with c = { x = circle.c.x + dx; y = circle.c.y + dy } }
  | Rectangle rectangle -> Rectangle { rectangle with c = { x = rectangle.c.x + dx; y = rectangle.c.y + dy } }
  | Ellipse ellipse -> Ellipse { ellipse with c = { x = ellipse.c.x + dx; y = ellipse.c.y + dy } }
  | Triangle triangle ->
      Triangle { p1 = { x = triangle.p1.x + dx; y = triangle.p1.y + dy };
                 p2 = { x = triangle.p2.x + dx; y = triangle.p2.y + dy };
                 p3 = { x = triangle.p3.x + dx; y = triangle.p3.y + dy } }
  | Star star -> Star { star with center = { x = star.center.x + dx; y = star.center.y + dy } }

let show shapes =
  open_graph " 500x500";
  set_color black;
  List.iter render_shape shapes;
  ignore (read_line ());
  close_graph ()
