(* JS deps *)
module Html = Js_of_ocaml.Dom_html
module Dom = Js_of_ocaml.Dom
module Js = Js_of_ocaml.Js
module G = Graphics_js

type point = { x : float; y : float }
type circle = { c : point; radius : float }
type rectangle = { c : point; width : float; height : float }

(* JS type conversion helpers *)
let str = Js.string
let bl = Js.bool

(* aliases for globals *)
let doc = Html.document
let window = Html.window

let draw_circle ctx { c; radius } =
  let { x; y } = c in
  ctx##beginPath;
  ctx##arc x y radius 0. (2. *. Float.pi) (bl false);
  ctx##stroke

let draw_rect ctx { c; width; height } = ctx##strokeRect c.x c.y width height

let get_window_size () =
  let w = float_of_int window##.innerWidth in
  let h = float_of_int window##.innerHeight in
  (w, h)

let create_canvas () =
  let w, h = get_window_size () in
  let canvas = Html.createCanvas doc in
  canvas##.width := int_of_float w;
  canvas##.height := int_of_float h;
  canvas

let render ctx =
  let w, h = get_window_size () in
  let x, y = (w /. 2., h /. 2.) in
  ctx##.fillStyle := str "white";
  ctx##fillRect 0. 0. w h;
  ctx##.fillStyle := str "black";
  let c = { c = { x; y }; radius = 50. } in
  let r = { c = { x = x -. 75.; y = y -. 75. }; width = 150.; height = 150. } in
  draw_circle ctx c;
  draw_rect ctx r

let onload _ =
  let canvas = create_canvas () in
  G.open_canvas canvas;
  Dom.appendChild doc##.body canvas;
  let context = canvas##getContext Html._2d_ in
  render context;
  Js._true

let _ = window##.onload := Html.handler onload
