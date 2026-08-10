open Joy

(*
   Demonstration of the repeat function.
   Takes n, a transformation, and an initial shape, and applies the
   transformation iteratively n times.

   Adapted from the original Joy python library's examples.
*)

let () =
  init "canvas";
  let c = circle ~c:(point (-100) 0) 50 in
  let shapes = repeat 10 (translate 10 0) c in
  show [ shapes ]
