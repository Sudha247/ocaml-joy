open Js_of_ocaml

let canvas_size : (int * int) ref = ref (500, 500)
let dom_elt : Dom_html.divElement Js.t option ref = ref None

module Svg_backend = struct
  type output = string

  let render shapes = Backend_svg.render ~size:!canvas_size shapes
end

include Base_joy.Make (Svg_backend)

module Noise = Noise

let init ?(size = (500, 500)) ?(axes = false) elt_id =
  canvas_size := size;
  let elt =
    Js.Opt.get
      (Js.Opt.bind
         (Dom_html.document##getElementById (Js.string elt_id))
         Dom_html.CoerceTo.div)
      (fun _ -> failwith "Could not find element with id")
  in
  dom_elt := Some elt;
  if axes then
    let half_w, half_h =
      size |> Util.tmap float_of_int |> Util.tmap (fun x -> x /. 2.0)
    in
    let gray = Color.color 128 128 128 ~a:0.5 in
    let x_axis =
      line ~a:{ x = -.half_w; y = 0. } { x = half_w; y = 0. }
      |> with_stroke gray
    in
    let y_axis =
      line ~a:{ x = 0.; y = -.half_h } { x = 0.; y = half_h }
      |> with_stroke gray
    in
    show [ x_axis; y_axis ]

let show new_shapes =
  show new_shapes;
  match !dom_elt with
  | Some elt -> elt##.innerHTML := Js.string (render ())
  | None -> ()

let clear () =
  clear ();
  match !dom_elt with
  | Some elt -> elt##.innerHTML := Js.string ""
  | None -> ()

let render () = render ()
