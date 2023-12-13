open Joy.Shape

(* Higher order transformations can be composed with `comp`,
   which applies its function args right-to-left.
   This allows us to create complex series transformations,
   that can be applied iteratively *)
let transform = compose (translate 10 10) (scale 0.9)
let rec range a b = if a > b then [] else a :: range (a + 1) b

let () =
  init ();
  let initial = rectangle ~point:(point (-250) (-250)) 100 100 in
  let match_list l =
    match l with
    | [] -> [ transform initial ]
    | last :: _ -> transform last :: l
  in
  let shapes = List.fold_right (fun _ acc -> match_list acc) (range 0 32) [] in
  show shapes;
  close ()
