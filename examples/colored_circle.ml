open Shape

let () =
  init ();

  let my_colored_circle = circle ~x:250 ~y:250 ~fill:(RGB (255, 0, 0)) ~stroke:(RGB (0, 255, 0)) 100 in

  show [my_colored_circle];

  ignore (read_line ());
  close ();