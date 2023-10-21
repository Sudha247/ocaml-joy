open Joy.Shape

let () =
  init ();

  (* Create ellipse *)
  let e1 = ellipse 60 30 in
  (* Translate it to the right by 100 and up by 50 *)
  let e2 = translate 100 50 e1 in
  (* Display both ellipses *)
  show [ e1; e2 ];

  close ()
