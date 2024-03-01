(* point -> point arithmetic *)
open Shape

let ( /~ ) p1 p2 = { x = p1.x /. p2.x; y = p1.x /. p2.x }

(* point -> scalar arithmetic *)
let ( -! ) { x; y } scalar = { x = x -. scalar; y = y -. scalar }
let ( /! ) { x; y } scalar = { x = x /. scalar; y = y /. scalar }
let ( *! ) { x; y } scalar = { x = x *. scalar; y = y *. scalar }
let pmap f { x; y } = { x = f x; y = f y }

(* Tuple/Vector ops *)
let tmap f (x, y) = (f x, f y)
let tmap3 f (a, b, c) = (f a, f b, f c)
let tmap4 f (a, b, c, d) = (f a, f b, f c, f d)

(** Function composition *)
let ( >> ) f g x = g (f x)

(* Partitions point in a polygon into faces *)
let rec take n lst =
  match (n, lst) with
  | 0, _ -> ([], lst)
  | _, [] -> ([], [])
  | n, x :: xs ->
      let taken, rest = take (n - 1) xs in
      (x :: taken, rest)

let rec partition n ?(step = 0) lst =
  match lst with
  | [] -> []
  | lst' ->
      let taken, _ = take n lst in
      if List.length taken = n then taken :: partition n ~step (List.tl lst')
      else []

(* Misc *)
let range n = List.init n Fun.id
