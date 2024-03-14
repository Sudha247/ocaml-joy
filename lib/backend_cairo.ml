type context = { dummy: string; size: int * int }

let string_of_color color =
  let r, g, b, a = color in
  Printf.sprintf "(%d, %d, %d, %f)" r g b a

let create ~background_color ~size ~line_width ~axes =
  let s = Printf.sprintf "create: background_color=%s size=(%d, %d) line_width=%d axes=%b\n"
    (string_of_color background_color) (fst size) (snd size) line_width axes in
  { dummy = s; size = size }

let show _ctx _shapes = ()

let set_line_width _ctx _line_width = ()

let write _ctx _filename = ()

let clear _ctx = ()
