open Graphics

type point = { x : int; y : int }

type star = {
  center : point;
  outer_radius : int;
  inner_radius : int;
  num_points : int;
}

type shape = Star of star

let canvas_size = (500, 500)

let render_shape s =
  match s with
  | Star star ->
      let center_x = star.center.x in
      let center_y = star.center.y in
      let outer_r = float_of_int star.outer_radius in
      let inner_r = float_of_int star.inner_radius in
      let num_points = star.num_points in
      let angle_step = 360.0 /. float_of_int num_points in
      moveto (center_x + int_of_float outer_r) center_y;
      for i = 1 to num_points * 2 do
        let angle = float_of_int i *. angle_step in
        let radius = if i mod 2 = 0 then outer_r else inner_r in
        let x =
          center_x + int_of_float (radius *. cos (angle *. 3.14159265 /. 180.0))
        in
        let y =
          center_y + int_of_float (radius *. sin (angle *. 3.14159265 /. 180.0))
        in
        lineto x y
      done;
      lineto (center_x + int_of_float outer_r) center_y

let star center outer_radius inner_radius num_points =
  Star { center; outer_radius; inner_radius; num_points }

let show shapes = List.iter render_shape shapes

let () =
  open_graph
    (" "
    ^ string_of_int (fst canvas_size)
    ^ "x"
    ^ string_of_int (snd canvas_size));
  set_color black;

  let star_shape = star { x = 250; y = 250 } 50 20 5 in

  show [ star_shape ];

  ignore (read_line ());
  close_graph ()
