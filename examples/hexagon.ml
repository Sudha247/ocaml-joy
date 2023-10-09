open Graphics

type point = { x : int; y : int }
type hexagon = { c : point; side_length : int}

type shape = Hexagon of hexagon

let canvas_size = (500, 500)
let canvas_mid = { x = fst canvas_size / 2; y = snd canvas_size / 2 }

let render_shape s =
  match s with
  | Hexagon hex ->
      let side = hex.side_length in
      let x = hex.c.x in
      let y = hex.c.y in
      let points =
        [|(x + (side/2), y + side);
          (x - (side/2), y + side);
          (x - (side), y);
          (x - (side/2), y - side);
          (x + (side/2), y - side);
          (x + side, y);|]
      in
      draw_poly points


let hexagon ?x ?y side_length =
  let center = match (x, y) with
    | Some x, Some y -> { x; y }
    | _ -> canvas_mid in
  Hexagon { c = center; side_length }

let show shapes = List.iter render_shape shapes

let () =
  open_graph (" " ^ string_of_int (fst canvas_size) ^ "x" ^ string_of_int (snd canvas_size));
  set_color black;

  let hex = hexagon 100 in
  show [hex];

  ignore (read_line ());
  close_graph ()
