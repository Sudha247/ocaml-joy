module type S = sig
  type output

  val render : Shape.shape list -> output
end
