open Graphics

type point = { x : int; y : int }
type pentagon = { c : point; side_length : int}

type shape = Pentagon of pentagon

let canvas_size = (500, 500)
let canvas_mid = { x = fst canvas_size / 2; y = snd canvas_size / 2 }

let render_shape s =
  match s with
  | Pentagon pent ->
      let side = pent.side_length in
      let x = pent.c.x in
      let y = pent.c.y in
      let points =
        [|(x, y + side);
          (x - side, y);
          (x - (side/2), y - side);
          (x + (side/2), y - side);
          (x + side, y);|]
      in
      draw_poly points


let pentagon ?x ?y side_length =
  let center = match (x, y) with
    | Some x, Some y -> { x; y }
    | _ -> canvas_mid in
  Pentagon { c = center; side_length }

let show shapes = List.iter render_shape shapes

let () =
  open_graph (" " ^ string_of_int (fst canvas_size) ^ "x" ^ string_of_int (snd canvas_size));
  set_color black;

  let pent = pentagon 100 in
  show [pent];

  ignore (read_line ());
  close_graph ()
