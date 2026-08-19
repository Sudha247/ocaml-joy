open Joy_core
open Shape

let run () =
  let r1 = rectangle 50 30 in
  let r2 = rectangle 100 60 in
  let _ = Backend_svg.render ~size:(500, 500) [ r1; r2 ] in
  ()
