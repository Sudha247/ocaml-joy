module Make (B : Backend.S) = struct
  include Shape
  include Transform
  include Color

  type output = B.output

  let shapes : shape list ref = ref []

  let show new_shapes = shapes := !shapes @ new_shapes

  let render () = B.render !shapes

  let clear () = shapes := []
end
