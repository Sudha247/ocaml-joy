(* open Graphics

   type point = { x : int; y : int }
   type rectangle = { c : point; length : int; width : int }
   type circle = { c : point; radius : int }
   type shape = Circle of circle | Rectangle of rectangle
   type _shapes = shape list

   let render_shape s =
     match s with
     | Circle circle -> draw_circle circle.c.x circle.c.y circle.radius
     | Rectangle rectangle ->
         draw_rect rectangle.c.x rectangle.c.y rectangle.length rectangle.width

   let circle ?x ?y r =
     match (x, y) with
     | Some x, Some y -> Circle { c = { x; y }; radius = r }
     | _ -> Circle { c = { x = 450; y = 450 }; radius = r }

   let _rectangle ?x ?y length width =
     match (x, y) with
     | Some x, Some y -> Rectangle { c = { x; y }; length; width }
     | _ -> Rectangle { c = { x = 150; y = 150 }; length; width }

   let show shapes = List.iter render_shape shapes *)

open Graphics

type point = { x : int; y : int }
type circle = { c : point; radius : int }
type shape = Circle of circle

let canvas_mid = { x = 250; y = 250 }

let render_shape s =
  match s with
  | Circle circle -> Graphics.draw_circle circle.c.x circle.c.y circle.radius

let circle ?x ?y r =
  match (x, y) with
  | Some x, Some y -> Circle { c = { x; y }; radius = r }
  | _ -> Circle { c = { x = canvas_mid.x; y = canvas_mid.y }; radius = r }

let show shapes = List.iter render_shape shapes

(* let () =
   open_graph " 500x500";
   (* Open a graphics window with dimensions 800x600 *)
   set_color black;

   let c1 = circle 100 in
   let c2 = circle 200 in
   show [ c1; c2 ];

   ignore (read_line ());
   close_graph () *)

let () =
  Graphics.open_graph " 500x500";
  set_color Graphics.black;
  let c1 = circle 50 in
  let c2 = circle 100 in
  show [ c1; c2 ];
  ignore (read_line ());
  (*wait for prompt from user before closing*)
  Graphics.close_graph ()
