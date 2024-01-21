open Joy

(* Constants *)
let size = 800.
let half_size = size /. 2.
let max_leaf_points = 3
let num_points = 900

(* Init rng *)
let _ = Random.self_init ()

(* Point utils *)
let pmap2 f ({x = x1; y = y1}: point) ({ x = x2; y = y2}: point): point = { x = f x1 x2; y = f y1 y2}
let ( /! ) ({ x; y}: point) scalar: point = { x = x /. scalar; y = y /. scalar }

let rand_point _: point = { x = (Random.float size) -. half_size; y = (Random.float size) -. half_size}

(* Box and utils *)
type box = { min : point; max: point }
let box (minx, miny) (maxx, maxy) = 
  { min = { x = minx; y = miny }; max = { x = maxx; y = maxy }}

let midpoint { min; max } = 
  (pmap2 (+.) min max) /! 2.

let quarters (mid: point) box =
  let lu = { min = { x = box.min.x; y = mid.y }; max = { x = mid.x; y = box.max.y } } in 
  let ru = { min = { x = mid.x; y = mid.y }; max = { x = box.max.x; y = box.max.y } } in 
  let rd = { min = { x = mid.x; y = box.min.y }; max = { x = box.max.x; y = mid.y } } in 
  let ld = { min = box.min; max = mid } in
  (lu, ru, rd, ld)

(* let (|=) box ({x; y}: point) = 
  { min = { x = min box.min.x x; y = min box.min.y y }; max = { x = max box.max.x x; y = max box.max.y y }}

let bound_points = List.fold_left (fun acc p -> acc |= p) *)

let contains box ({ x;y }: point) = 
  x > box.min.x && x < box.max.x && y > box.min.y && y < box.max.y

(* Quadtree and utils *)
type 'a leaf = box * 'a list
type 'a tree = Empty | Leaf of 'a leaf | Node of { aabb : box; children : 'a tree list }

let split_root box points = 
  let partition (lu, ru, rd, ld) es = 
    let belong box = List.filter (contains box) in
    (
      (lu, (belong lu es)), 
      (ru, (belong ru es)),
      (rd, (belong rd es)), 
      (ld, (belong ld es))
    )
  in
  let rec split (box, es) = 
     if List.length es > max_leaf_points then 
        let mid = midpoint box in
        let quarters' = quarters mid box in 
        let (lu, ru, rd,ld) = partition quarters' points in
        Node { aabb = box; children = List.map split [lu; ru; rd; ld]}
      else 
        Leaf (box, es)  
  in 
  split (box, points)


let build () = 
  let _ = Empty in
  let root = box ((-.half_size), (-. half_size)) (half_size, half_size) in 
  let points = List.init num_points rand_point in 
  split_root root points

let to_flat_shapes tree: shape list =
  let rect_of_bb bb = rectangle ~c:(midpoint bb) (bb.max.x -. bb.min.x) (bb.max.y -. bb.min.y) in
  let circle_of_point pt = 
    circle ~c:pt 1. 
  in
  let rec convert xs = function 
    | Node { aabb; children } -> 
      let b = rect_of_bb aabb in
      List.flatten (List.map (convert (b :: xs)) children) 
    | Leaf (aabb, es) -> 
      let b = rect_of_bb aabb in
      ((List.map circle_of_point es) @ (b :: xs) )
    | Empty -> 
      [] 
  in
  convert [] tree

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

