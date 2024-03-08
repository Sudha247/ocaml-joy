type point = float Joy.point

(* Constants *)
let size = 800.
let half_size = size /. 2.
let max_leaf_points = 4
let clusters = 32
let point_size = 1
let box_color = (0, 0, 0)
let point_color = (255, 1, 1)

(* Point utils *)
let splat n : point = { x = n; y = n }

let pmap2 f ({ x = x1; y = y1 } : point) ({ x = x2; y = y2 } : point) : point =
  { x = f x1 x2; y = f y1 y2 }

let ( +~ ) (p1 : point) (p2 : point) : point =
  { x = p1.x +. p2.x; y = p1.y +. p2.y }

let ( /! ) ({ x; y } : point) scalar : point =
  { x = x /. scalar; y = y /. scalar }

(* Random utils for creating random clustered points *)
let rand_point () : point =
  { x = Joy.frandom size -. half_size; y = Joy.frandom size -. half_size }

(* Creates a point within 50 units of a center *)
let centered_point (center : point) _ : point =
  let offset () = Joy.frandom 100. -. 50. in
  center +~ { x = offset (); y = offset () }

(* Creates a list of random points clustered around a center point *)
let cluster _ =
  let center = rand_point () in
  List.init (8 + Joy.random 24) (centered_point center)

(* Box and utils *)

(* Axis aligned bounding box *)
type box = { min : point; max : point }

let box min max = { min; max }

(* Returns the middle point of the box *)
let midpoint { min; max } = pmap2 ( +. ) min max /! 2.

(* Subdivides a box into four even axis-aligned boxes *)
let quarters ({ min; max } as box) =
  let mid = midpoint box in
  let lu = { min = { x = min.x; y = mid.y }; max = { x = mid.x; y = max.y } } in
  let ru = { min = { x = mid.x; y = mid.y }; max = { x = max.x; y = max.y } } in
  let rd = { min = { x = mid.x; y = min.y }; max = { x = max.x; y = mid.y } } in
  let ld = { min; max = mid } in
  (lu, ru, rd, ld)

(* Checks whether point is within bounds of box *)
let contains { min; max } ({ x; y } : point) =
  x > min.x && x < max.x && y > min.y && y < max.y

(* Quadtree and utils *)

(* 2-tuple of bounding box * 'a list of elts whose positions are within that box *)
type 'a leaf = box * 'a list
type 'a tree = Leaf of 'a leaf | Node of 'a tree list

(* Constructs tree from root *)
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

(* Builds our float point tree *)
let build () =
  let root = box (splat (-.half_size)) (splat half_size) in
  let points = List.flatten (List.init clusters cluster) in
  split_root root points

(* Converts our constructed tree into a flat list of shapes for rendering *)
let to_flat_shapes tree =
  let open Joy in
  (* Converts box into rectangle *)
  let rect_of_bb bb =
    rectangle ~c:(midpoint bb)
      (int_of_float (bb.max.x -. bb.min.x))
      (int_of_float (bb.max.y -. bb.min.y))
    |> with_stroke box_color
  in
  (* Converts point into circle of radius 1 *)
  let circle_of_point pt = circle ~c:pt point_size |> with_stroke point_color in
  (* Traverses tree recursively *)
  let rec convert xs = function
    | Node children -> List.concat_map (convert xs) children
    | Leaf (aabb, es) ->
        let b = rect_of_bb aabb in
        List.map circle_of_point es @ (b :: xs)
  in
  convert [] tree

let () =
  let open Joy in
  init ();
  let tree = build () in
  let shapes = to_flat_shapes tree in
  show shapes;
  write ~filename:"quadtree.png" ()
