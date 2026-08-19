let current_ctx : Backend_cairo.context option ref = ref None

let get_ctx () =
  match !current_ctx with
  | Some c -> c
  | None -> failwith "No context initialized. Call init first."

module Cairo_backend = struct
  type output = unit

  let render shapes = Backend_cairo.render (get_ctx ()) shapes
end

include Joy_core.Base_joy.Make (Cairo_backend)

module Noise = Joy_core.Noise

let init ?(background_color = Joy_core.Color.white) ?(line_width = 2)
    ?(size = (500, 500)) ?(axes = false) () =
  let ctx = Backend_cairo.create ~background_color ~line_width ~size () in
  current_ctx := Some ctx;
  if axes then
    let half_w, half_h =
      size |> Joy_core.Util.tmap float_of_int
      |> Joy_core.Util.tmap (fun x -> x /. 2.0)
    in
    let gray = Joy_core.Color.color 128 128 128 ~a:0.5 in
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
  render ()

let clear () =
  clear ();
  Backend_cairo.repaint_background (get_ctx ())

let set_line_width n = Backend_cairo.set_line_width (get_ctx ()) n

let write filename = Backend_cairo.write (get_ctx ()) filename
