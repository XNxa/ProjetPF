module type Quadtree = 
(* General interface for a quadtree *) 
sig

  type 'a quadtree = 
    | Empty
    | Leaf of 'a
    | Node of 'a quadtree * 'a quadtree * 'a quadtree * 'a quadtree

  (* Get the element at position x y *)
  val at : int -> int -> 'a quadtree -> 'a option
  (* add or replace element at position x y*)
  val insert : int -> int -> 'a -> 'a quadtree -> 'a quadtree
  (* remove element in position x y *)
  val remove : int -> int -> 'a quadtree -> 'a quadtree
  (* print an int quadtree for debug purposes *)
  val print_int_qt : int quadtree -> unit
  val all_elements_and_coordinates : 'a quadtree -> ('a*(int*int)) list
end

module type Screen_2D =
sig
  val width : float
  val height : float
end

module type SizedBrick = 
sig
  type brick = Brick of float * float * float * float
  val get_x : brick -> float
  val get_y : brick -> float
  val get_w : brick -> float 
  val get_h : brick -> float
end

module BrickQuadtree(Screen : Screen_2D)(Brick : SizedBrick) =
struct
  (* 
     L'espace 2D est divisé dans le quadtree comme cela :
    Node (a, b, c, d)
           |
       c   |   d
           |
    -------|--------   
           |
       a   |   b
           |
   et ce recursivement
  *)

  open Brick

  type 'a quadtree = 
    | Empty
    | Leaf of 'a
    | Node of 'a quadtree * 'a quadtree * 'a quadtree * 'a quadtree

  let approx a b = Float.abs (a -. b) <= 100.*.Float.epsilon
  let approxb (Brick(ax, ay, _, _)) bx by =  approx ax bx && approx ay by

  let at x y quadtree = 
    let rec at_aux n accx accy quadtree =
      match quadtree with 
      | Empty -> None
      | Leaf b -> if get_x b = x && get_y b = y then Some b else None
      | Node (a, b, c, d) ->
        let m1, m2 = Screen.width /. 2.**n, Screen.height /. 2.**n in
        if x -. accx < m1 then 
          if y -. accy < m2 then 
            at_aux (n+.1.) accx accy a
          else 
            at_aux (n+.1.) accx (accy+.m2) c
        else 
          if y -. accy < m2 then 
            at_aux (n+.1.) (accx+.m1) accy b
          else 
            at_aux (n+.1.) (accx+.m1) (accy+.m2) d
      in
        at_aux 1. 0. 0. quadtree 

  let insert v quadtree = 
    let rec insert_aux n accx accy v quadtree =
      let x = get_x v and y = get_y v in
      match quadtree with 
      | Empty -> (Leaf v) 
      | Leaf b -> 
        let tree_with_b = insert_aux n accx accy b (Node (Empty, Empty, Empty, Empty))
        in 
          (insert_aux n accx accy v tree_with_b)
      | Node (a, b, c, d) -> 
        let m1, m2 = (Screen.width /. 2.**n), (Screen.height /. 2.**n) in
        if x -. accx < m1 then
          if y -. accy < m2 then
            (Node (insert_aux (n+.1.) accx accy v a, b, c, d))
          else
            (Node (a, b, insert_aux (n+.1.) accx (accy+.m2) v c, d))
        else
          if y -. accy < m2 then
            (Node (a, insert_aux (n+.1.) (accx+.m1) accy v b, c, d))
          else 
            (Node (a, b, c, insert_aux (n+.1.) (accx+.m1) (accy+.m2) v d))
    in
      insert_aux 1. 0. 0. v quadtree


  let remove x y quadtree = 
    let normalize q = 
      match q with 
      | Node (Empty, Empty, Empty, Empty) -> Empty
      | Node (Leaf a, Empty, Empty, Empty) -> Leaf a
      | Node (Empty, Leaf a, Empty, Empty) -> Leaf a
      | Node (Empty, Empty, Leaf a, Empty) -> Leaf a
      | Node (Empty, Empty, Empty, Leaf a) -> Leaf a
      | _ -> q
    in 
    let rec remove_aux n accx accy quadtree = 
      match quadtree with
       | Empty -> Empty
       | Leaf _ -> (* if approxb b x y then Empty else Leaf b *) Empty
       | Node (a, b, c, d) -> 
          let m1, m2 = (Screen.width /. 2.**n), (Screen.height /. 2.**n) in
          if x -. accx < m1 then
            if y -. accy < m2 then
              normalize (Node (remove_aux (n+.1.) accx accy a, b, c, d))
            else
              normalize (Node (a, b, remove_aux (n+.1.) accx (accy+.m2) c, d))
          else
            if y -. accy < m2 then
              normalize (Node (a, remove_aux (n+.1.) (accx+.m1) accy b, c, d))
            else 
              normalize (Node (a, b, c, remove_aux (n+.1.) (accx+.m1) (accy+.m2) d))
    in 
      remove_aux 1. 0. 0. quadtree



  (* let print_int_qt quadtree =
    match quadtree with
    | Empty -> print_endline "Empty";
    | Leaf b -> print_string "Brick"; print_float (get_x b); print_float (get_y b); print_newline ();
    | Node _ -> print_endline "NEW NODE" 


  let all_elements_and_coordinates quadtree = failwith "Not implemented" *)
end

module Tests =
struct

  module Ecran : Screen_2D =
  struct
    let width = 100.
    let height = 200.
  end

  module Brique : SizedBrick = 
  struct
    type brick = Brick of float * float * float * float
    let get_x b = let Brick (x, _, _, _) = b in x
    let get_y b = let Brick (_, x, _, _) = b in x
    let get_w b = let Brick (_, _, x, _) = b in x
    let get_h b = let Brick (_, _, _, x) = b in x
  end

  module BQT = BrickQuadtree(Ecran)(Brique)
  open BQT
  open Brique


  let q1 = insert (Brick (1., 2., 10., 10.)) Empty
  let q2 = insert (Brick (51., 102., 10., 10.)) q1

  let%test _ = q1 = Leaf (Brick (1., 2., 10., 10.))
  let%test _ = at 1. 2. q2 = Some (Brick (1., 2., 10., 10.))
  let%test _ = at 5. 2. q2 = None
  let%test _ = at 56. 2. q2 = None
  let%test _ = at 51. 102. q2 = Some (Brick (51., 102., 10., 10.))
  let%test _ = remove 51. 102. q2 = q1
  let%test _ = remove 52. 103. q2 = q1


end