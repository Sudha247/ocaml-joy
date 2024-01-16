open Joy

let () =
  init ();
<<<<<<< HEAD
  let c = circle 100 in
=======
  let c = circle 100. in
>>>>>>> eaf1693 (Fix scaling factor in scale transform (#88))
  let hole = scale 0.5 c in
  show [ c; hole ];
  write ~filename:"donut_with_scale.png" ()
