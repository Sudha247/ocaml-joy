open Cairo

type cairo_context = {
  ctx: context; 
  surface: Surface.t
}

let context : cairo_context option ref = ref None

let draw () = 
  match !context with 
  | Some {ctx; surface} ->  
    Cairo.set_line_width ctx 0.01;
    Cairo.set_source_rgb ctx 0. 0. 0.;
    Cairo.rectangle ctx 0.25 0.25 ~w: 0.5 ~h: 0.5;
    Cairo.stroke ctx;

    Cairo.set_source_rgb ctx 0. 0. 0.;
    Cairo.arc ctx 0.5 0.5 ~r:0.25 ~a1: 0. ~a2: (2. *. Float.pi);
    Cairo.stroke ctx;
    Cairo.PNG.write surface "test2.png"
  | None -> failwith "Context not found"

let init ?size () = 
  let w, h = match size with | Some s -> s | None -> (800., 800.) in
  let surface = Cairo.Image.create Cairo.Image.ARGB32 ~w: (int_of_float w) ~h: (int_of_float h) in
  let ctx = (Cairo.create surface) in
  Cairo.scale ctx w h;
  Cairo.set_source_rgb ctx 1. 1. 1.;
  Cairo.paint ctx;
  context := Some {ctx; surface};
  draw ()

let () = 
  init ()