type color = int * int * int

let black = (0, 0, 0)
let white = (255, 255, 255)
let red = (255, 1, 1)
let green = (1, 255, 1)
let blue = (1, 1, 255)
let yellow = (255, 255, 1)
let transparent = (0, 0, 0, 0)
let opaque (r, g, b) = (r, g, b, 255)
