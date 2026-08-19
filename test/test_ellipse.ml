open Joy_core
open Shape

let run () =
  let e1 = ellipse 50 30 in
  let e2 = ellipse 100 60 in
  let _ = Backend_svg.render ~size:(500, 500) [ e1; e2 ] in
  ()
