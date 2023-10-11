open Joy.Shape

let () =
  init ();

  (* Create rectangle transform *)
  let r1 = rectangle 200 100 in
  let r2 = translate 100 0 r1 in

  (* Display rectangle transform *)
  show [r1; r2];

  close ()