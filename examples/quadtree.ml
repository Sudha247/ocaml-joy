open Joy

type point = Joy.point

(* Constants *)
let size = 800.
let half_size = size /. 2.
let max_leaf_points = 4
let clusters = 32

(* Init rng *)
let _ = Random.self_init ()

(* Point utils *)
let splat n = point n n

let pmap2 f ({ x = x1; y = y1 } : point) ({ x = x2; y = y2 } : point) =
  point (f x1 x2) (f y1 y2)

let ( +~ ) (p1 : point) (p2 : point) : point =
  point (p1.x +. p2.x) (p1.y +. p2.y)

let ( /! ) ({ x; y } : point) scalar : point =
  { x = x /. scalar; y = y /. scalar }

(* Random utils for creating random clustered points *)
let rand_point () =
  point (Random.float size -. half_size) (Random.float size -. half_size)

let centered_point (center : point) _ : point =
  let offset () = Random.float 100. -. 50. in
  center +~ { x = offset (); y = offset () }

let cluster _ =
  let center = rand_point () in
  List.init (8 + Random.int 24) (centered_point center)

(* Box and utils *)
type box = { min : point; max : point }
(** Axis aligned bounding box *)

let box min max = { min; max }

(** Returns the middle point of the box *)
let midpoint { min; max } = pmap2 ( +. ) min max /! 2.

(** Subdivides a box into four even axis-aligned boxes *)
let quarters ({ min; max } as box) =
  let mid = midpoint box in
  let lu = { min = { x = min.x; y = mid.y }; max = { x = mid.x; y = max.y } } in
  let ru = { min = { x = mid.x; y = mid.y }; max = { x = max.x; y = max.y } } in
  let rd = { min = { x = mid.x; y = min.y }; max = { x = max.x; y = mid.y } } in
  let ld = { min; max = mid } in
  (lu, ru, rd, ld)

(** checks whether point is within bounds of box *)
let contains { min; max } ({ x; y } : point) =
  x > min.x && x < max.x && y > min.y && y < max.y

(* Quadtree and utils *)
type 'a leaf = box * 'a list
(** Leaf is 2-tuple of bounding box * 'a list of elts whose position is within that box *)

type 'a tree = Leaf of 'a leaf | Node of 'a tree list
(* Node potentially doesn't need to hold aabb? *)

(** Constructs tree from root *)
let split_root box points =
  (* Groups points with the boxes that contain them *)
  let partition (lu, ru, rd, ld) es =
    let belongs box = List.filter (contains box) in
    ( (lu, belongs lu es),
      (ru, belongs ru es),
      (rd, belongs rd es),
      (ld, belongs ld es) )
  in
  (* Splits and converts to Node if leaf has too many points,
      otherwise returns leaf *)
  let rec split (box, es) =
    if List.length es > max_leaf_points then
      let quarters' = quarters box in
      let lu, ru, rd, ld = partition quarters' points in
      Node (List.map split [ lu; ru; rd; ld ])
    else Leaf (box, es)
  in
  split (box, points)

let build () =
  let root = box (splat (-.half_size)) (splat half_size) in
  let points = List.flatten (List.init clusters cluster) in
  split_root root points

let to_flat_shapes tree =
  let rect_of_bb bb =
    rectangle ~c:(midpoint bb) (bb.max.x -. bb.min.x) (bb.max.y -. bb.min.y)
  in
  let circle_of_point pt = circle ~c:pt 1. in
  let rec convert xs = function
    | Node children -> List.flatten (List.map (convert xs) children)
    | Leaf (aabb, es) ->
        let b = rect_of_bb aabb in
        List.map circle_of_point es @ (b :: xs)
  in
  convert [] tree

(* With color handling system this function won't be necessary as color can be
   decided at construction *)
let render_color shape =
  match shape with
  | Shape.Circle _ ->
      set_color (1., 1. /. 255., 1. /. 255.);
      render shape
  | _ ->
      set_color (0., 0., 0.);
      render shape

let () =
  init ();
  background (1., 1., 1., 1.);
  let tree = build () in
  let to_shapes = to_flat_shapes tree in
  set_color (0., 0., 0.);
  List.iter render_color to_shapes;
  write ~filename:"quadtree.png" ()
