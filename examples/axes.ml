open Joy

let _ =
  init ~axes:true ();
  let c = circle 50. in
  render c;
  exit 0
