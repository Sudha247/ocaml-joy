open Graphics

type point = { x : int; y : int }
type triangle = { p1 : point; p2 : point; p3 : point }

type shape = Triangle of triangle

let canvas_size = (500, 500)

let render_shape s =
  match s with
  | Triangle tri ->
      let x1 = tri.p1.x in
      let y1 = tri.p1.y in
      let x2 = tri.p2.x in
      let y2 = tri.p2.y in
      let x3 = tri.p3.x in
      let y3 = tri.p3.y in
      moveto x1 y1;
      lineto x2 y2;
      lineto x3 y3;
      lineto x1 y1

let triangle p1 p2 p3 =
  Triangle { p1; p2; p3 }

let show shapes = List.iter render_shape shapes

let () =
  Graphics.open_graph (" " ^ string_of_int (fst canvas_size) ^ "x" ^ string_of_int (snd canvas_size));
  set_color Graphics.black;

  let triangle_shape = triangle { x = 250; y = 400 } { x = 150; y = 200 } { x = 350; y = 200 } in

  show [triangle_shape];

  ignore (read_line ());
  Graphics.close_graph ()
