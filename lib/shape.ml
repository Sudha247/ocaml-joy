open Graphics

type point = { x : int; y : int }
type rectangle = { c : point; length : int; width : int }
type circle = { c : point; radius : int }
type ellipse = { c : point; rx : int; ry : int }
type shape = Circle of circle | Rectangle of rectangle | Ellipse of ellipse
type shapes = shape list

let canvas_mid = { x = 250; y = 250 }

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
  | Some x, Some y -> Ellipse { c = { x; y }; rx; ry }
  | _ -> Ellipse { c = { x = canvas_mid.x; y = canvas_mid.y }; rx; ry }

let translate dx dy shape =
  match shape with
  | Circle circle -> Circle { circle with c = { x = circle.c.x + dx; y = circle.c.y + dy } }
  | Rectangle rectangle -> Rectangle { rectangle with c = { x = rectangle.c.x + dx; y = rectangle.c.y + dy } }
  | Ellipse ellipse -> Ellipse { ellipse with c = { x = ellipse.c.x + dx; y = ellipse.c.y + dy } }

let show shapes = List.iter render_shape shapes

let init () =
  open_graph " 500x500";
  set_color black

let close () =
  ignore (read_line ());
  close_graph ()

let () =
  init ();
  set_color black;

  (* Create shapes *)
  let r1 = rectangle 200 100 in
  let r2 = translate 100 0 r1 in

  (* Display shapes *)
  show [r1; r2];

  close ()





(* open Graphics

type point = { x : int; y : int }
type rectangle = { c : point; length : int; width : int }
type circle = { c : point; radius : int }
type ellipse = {c : point; rx : int; ry: int}
type shape = Circle of circle | Rectangle of rectangle | Ellipse of ellipse
type shapes = shape list

let canvas_mid = { x = 250; y = 250}

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

let show shapes = List.iter render_shape shapes

let init () =
  open_graph " 500x500";
  set_color black

let close () =
  ignore (read_line ());
  close_graph () *)

