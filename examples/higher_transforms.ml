open Joy

(* Higher order transformations can be composed with `comp`,
   which applies its function args right-to-left.
   This allows us to create complex series transformations,
   that can be applied iteratively *)
let transform = compose (translate 10 10) (scale 0.9)

let () =
  init ();
  let initial = rectangle ~c:(point (-250) (-250)) 100 100 in
  let shapes = repeat 32 transform initial in
  render shapes;
  write ~filename:"higher_transforms.png" ()
