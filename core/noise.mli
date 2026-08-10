(** Simplex noise, output in range -1..1. Takes 1 or 2 coordinates. *)
val noise : float list -> float

(** Layered fractal noise, output in range -1..1. Takes 1 or 2 coordinates. *)
val fractal :
  ?octaves:int ->
  ?frequency:float ->
  ?amplitude:float ->
  ?lacunarity:float ->
  ?persistence:float ->
  float list -> float
