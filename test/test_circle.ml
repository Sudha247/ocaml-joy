open Joy_core
open Shape

let run () =
  let c1 = circle 50 in
  let c2 = circle 100 in
  let _ = Backend_svg.render ~size:(500, 500) [ c1; c2 ] in
  ()
