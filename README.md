# Joy

Joy is a creative coding library inspired by [Joy](https://github.com/fossunited/joy), 
with a focus on simplicity, elegance, and composability.

# Installation

Joy is currently in active development and hasn't been added to the Opam package 
registry, as such you will need to clone and install it manually.

```bash
git clone https://github.com/Sudha247/ocaml-joy && 
cd ocaml-joy && 
opam add .
```

# Getting started 

Like the library that inspired it, Joy uses a 'signed' coordinate system with 
`0,0` at the center of the canvas. The default canvas size is `500,500`, with the coordinates spanning from `-250` to `250` on each axis.

The SVG backend (the one you should use if you're unsure) can be opened in your
project with `open Joy.SVG` (which will be omitted from the snippets in this
document)

## A basic sketch 

The simplest Joy program, a circle rendered at the center of the screen looks
like this:

```ocaml
let () = 
    init ();
    let c = circle 100 in 
    show [ c ];
    write ()
```

Let's break this down. `init` does all the behind-the-scenes render magic that 
allows our shape to be drawn to an image file. `circle 100` creates a circle at 
position `0,0`, the default for shape constructor functions when a position is 
not passed, with radius 100. You could draw the circle 100 units above center 
like this:

`let c = circle ~c:(point 0 100) 100`

Notice the call to `show`, the circle is placed within a list. This is how we 
render shapes in Joy. Placing our shape arguments in a list allows us to draw 
any number of shapes at once without any awkward optional argument syntax. 

And finally `write` writes our digital canvas to a PNG file. `write` takes an
optional `filename` argument, like this `write ~filename:"example.png" ()`, so
that you can name your file something besides `joy.png`.

## Transformations

A core concept of Joy is the "transformation". These are functions that change 
the way the shape is drawn, its position, or rotation, or size. A simple 
transformation, let's start with rotation, could look like this:

```ocaml
let () = 
    init ();
    let rect = rectangle 100 100 in 
    let rotated = rotate 45 r in 
    show [ rect; rotated ];
    write ()
```

Transformations can also be composed together:

```ocaml
let rotate_and_shrink = compose (rotate 45) (scale 0.9)
```

This means that the result of the first transformation will be fed into the next.
Through this wecan build up complex custo transforations that behave just like 
the ones included in the library. 

And finally, transformations can be iterated: 

```ocaml
let rect = rectangle 100 in 
let spiral = repeat 16 (rotate 45) rect
```

This is how we build more complex shapes out of the simple primitives and 
transformations in the library. It may look a little intimidating if you are new
to functional programming, let me break it down. 

`repeat` takes three arguments. The first is the number of iterations, or how many
shapes you willl end up with, the second is the transformation you want to apply,
and the last is the initial shape. 

It takes that initial shape, applies the transformation you gave it, then 
*take the result of that* and applies the tranformation to *that*, repeatedly 
the number of times specified with the first argument, and put them all together 
(more on this later). You can think of it like a `fold`/`reduce` operation over 
a range.

There is an added benefit to this...

## Complex shapes

`repeat` returns a single shape, when you might expect an operation like that to 
return a list of shapes. It actually *sort* of does both, let me explain. 

In addition to the usual geometric primitives, Joy also has a `Complex` type. 
This is a list of shapes that act like a single shape. This means that 
transformations applied to a complex shape are applied uniformly to every shape 
contained in a complex shape. This is useful because it allows you to create 
more visually complex shapes and treat the the same as you would primitives, 
transforming and composing them. In addition to `repeat`, a complex shape can 
also be created like so:

```ocaml
let circle' = circle 100 in 
let rectangle' = rectangle 100 100 in 
let ellipse' = ellipse 100 70 in
let line' = line (point (-250) 250) in 
let complex' = complex [ circle', rectangle', ellipse', line' ]
```

# Going further

This documentation is a work in progress and additional tutorials and package 
docs will be released soon to cover topics such as rendering polygons, working 
with colors, classic generative art algorithms, `.svg` file rendering, and the
HTML canvas backend.