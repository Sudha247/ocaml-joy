open Joy.Shape

(* Higher order transformations can be composed with `comp`,
   which applies its function args right-to-left.
   This allows us to create complex series of transformations,
   that can be applied iteratively with the repeat function *)
let transform = compose (translate 10 10) (scale 0.9)

let () =
  init ();
  let initial = rectangle ~point:{x = (-250); y = (-250)} 100 100 in
  let shapes = repeat 32 transform initial in
  render_shape shapes;
  close ()
