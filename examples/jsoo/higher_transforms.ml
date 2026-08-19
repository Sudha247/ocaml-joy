open Joy

(* Higher order transformations can be composed with `compose`,
   which applies its function args left-to-right.
   This allows us to create complex series of transformations
   that can be applied iteratively. *)
let transform = compose (translate 10 10) (scale 0.9)

let () =
  init "canvas";
  let initial = rectangle ~c:(point (-250) (-250)) 100 100 in
  let shapes = repeat 32 transform initial in
  show [ shapes ]
