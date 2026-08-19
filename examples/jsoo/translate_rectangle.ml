open Joy

let () =
  init "canvas";
  (* Create rectangle transformation *)
  let r1 = rectangle 200 100 in
  let r2 = translate 100 0 r1 in
  (* Display rectangle transformation *)
  show [ r1; r2 ]
