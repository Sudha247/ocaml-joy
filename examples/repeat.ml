open Joy

(*
    demonstration of the repeat function
    takes n, an operation, and an initial shape, and applies the operation
    iteratively to the initial shape n times

    adapted from the original Joy python library's examples
*)

let () =
  init ();
  background (1., 1., 1., 1.);
  let circle = circle ~c:(point (-100.) 0.) 50. in
  let shapes = repeat 10 (translate 10. 0.) circle in
  set_color (0., 0., 0.);
  render shapes;
  write ~filename:"repeat.png" ()
