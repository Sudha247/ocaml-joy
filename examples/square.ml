open Graphics

type point = { x : int; y : int }
type square = { c : point; side_length : int}

type shape = Square of square

let canvas_size = (500, 500)
let canvas_mid = { x = fst canvas_size / 2; y = snd canvas_size / 2 }

let render_shape s =
  match s with
  | Square sqr ->
      let side = sqr.side_length in
      let x = sqr.c.x in
      let y = sqr.c.y in
      let points =
        [| (x - side, y - side);
           (x + side, y - side);
           (x + side, y + side);
           (x - side, y + side) |]
      in
      draw_poly points


let square ?x ?y side_length =
  let center = match (x, y) with
    | Some x, Some y -> { x; y }
    | _ -> canvas_mid in
  Square { c = center; side_length }

let show shapes = List.iter render_shape shapes

let () =
  open_graph (" " ^ string_of_int (fst canvas_size) ^ "x" ^ string_of_int (snd canvas_size));
  set_color black;

  let sqr = square 100 in
  show [sqr];

  ignore (read_line ());
  close_graph ()
