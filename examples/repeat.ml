open Joy.Shape

(*
    demonstration of the repeat function
    takes n, an operation, and an initial shape, and applies the operation
    iteratively to the initial shape n times

    adapted from the original Joy python library's examples
*)

let () =
  init ();
<<<<<<< HEAD
  let circle = circle ~point:{x = (-100); y = 0} 50 in 
  let shapes = repeat 10 (translate 10 0) circle in 
=======
  let circle = circle ~x:(-100) ~y:0 50 in
  let shapes = repeat 10 (translate 10 0) circle in
>>>>>>> 66df79e (formatting)
  render_shape shapes;
  close ()
