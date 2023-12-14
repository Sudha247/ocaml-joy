(* JS deps *)
module Html = Js_of_ocaml.Dom_html
module Dom = Js_of_ocaml.Dom
module Js = Js_of_ocaml.Js
module G = Graphics_js

type point = { x : float; y : float }
type ellipse = {c: point; rx: float; ry: float}

(* JS type conversion helpers *)
let str = Js.string
let bl = Js.bool

(* aliases for globals *)
let doc = Html.document
let window = Html.window

(* Context *)
let context : Html.canvasRenderingContext2D Js.t option ref = ref None
let fail () = window##alert (str "Rendering context not found")

let draw_ellipse ctx { c = {x; y}; rx = _rx; ry = _ry} = 
  ctx##scale 1. 0.5;
  ctx##beginPath;
  ctx##arc x y 200. 0. (2. *. Float.pi) (bl false);
  ctx##stroke

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

let render () =
  match !context with
  | Some ctx ->
      let w, h = get_window_size () in
      let x, y = (w /. 2., h /. 2.) in
      ctx##.fillStyle := str "white";
      ctx##fillRect 0. 0. w h;
      ctx##.fillStyle := str "black";
      let ellipse = {c = {x; y}; rx = 50.; ry = 75.}
      in
      draw_ellipse ctx ellipse
  | None -> fail ()

let onload _ =
  let canvas = create_canvas () in
  G.open_canvas canvas;
  Dom.appendChild doc##.body canvas;
  context := Some (canvas##getContext Html._2d_);
  render ();
  Js._true

let _ = window##.onload := Html.handler onload
