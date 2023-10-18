open Joy.Shape

let () =
  init ();

  (* Create a black circle *)
  let black_circle = circle 50 in

  (* Create a colored circle with a yellow fill and a red stroke *)
  let colored_circle = circle ~fill:(255, 255, 0) ~stroke:(255, 0, 0) 50 in

  (* Show both circles *)
  show [black_circle; colored_circle];

  close ()