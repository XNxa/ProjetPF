
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

end


module type Square_2D_Space =
(* Module to store a square plane size *)
sig
  val length : int
end

module Quadtree2D (Screen : Square_2D_Space) : Quadtree =
(* Implementation of a quadtree to store elements of Square surface *)
struct
  let rec log2 = function
  | 1 -> 0
  | n -> 1 + log2 (n / 2)

  let rec pow a = function
  | 0 -> 1
  | 1 -> a
  | n -> let b = pow a (n / 2) in
    b * b * (if n mod 2 = 0 then 1 else a)

  type 'a quadtree =
    | Empty
    | Leaf of 'a
    | Node of 'a quadtree * 'a quadtree * 'a quadtree * 'a quadtree

  let insert x y elem q =
    let n_max = log2 Screen.length in
    let rec insert_aux x y q n =
      match q with 
      | Empty ->  
        if n < n_max then
          let m = pow 2 (n_max - n) in
          if x <= m && y <= m then
            Node (insert_aux x y Empty (n+1), Empty, Empty, Empty)
          else if x > m && y <= m then
            Node (Empty, insert_aux (x-m) y Empty (n+1), Empty, Empty)
          else if x <= m && y > m then
            Node (Empty, Empty, insert_aux x (y-m) Empty (n+1), Empty)
          else
            Node (Empty, Empty, Empty, insert_aux (x-m) (y-m) Empty (n+1))
        else
          if x = 1 && y = 1 then
            Node (Leaf elem, Empty, Empty, Empty)
          else if x = 2 && y = 1 then
            Node (Empty, Leaf elem, Empty, Empty)
          else if x = 1 && y = 2 then
            Node (Empty, Empty, Leaf elem, Empty)
          else
            Node (Empty, Empty, Empty, Leaf elem)
      | Node (hg, hd, bg, bd) ->
        if n < n_max then
          let m = pow 2 (n_max - n) in
          if x <= m && y <= m then
            Node (insert_aux x y hg (n+1), hd, bg, bd)
          else if x > m && y <= m then
            Node (hg, insert_aux (x-m) y hd (n+1), bg, bd)
          else if x <= m && y > m then
            Node (hg, hd, insert_aux x (y-m) bg (n+1), bd)
          else
            Node (hg, hd, bg, insert_aux (x-m) (y-m) bd (n+1))
        else
          if x = 1 && y = 1 then
            Node (Leaf elem, hd, bg, bd)
          else if x = 2 && y = 1 then
            Node (hg, Leaf elem, bg, bd)
          else if x = 1 && y = 2 then
            Node (hg, hd, Leaf elem, bd)
          else
            Node (hg, hd, bg, Leaf elem)
      | Leaf _ -> failwith "Should not happen"

    in insert_aux x y q 1

    let at x y q =
      let n_max = log2 Screen.length in
      let rec at_aux x y q n =
        match q with 
        | Empty -> None
        | Leaf a -> Some a
        | Node (hg, hd, bg, bd) -> 
          let m = pow 2 (n_max - n) in
          if x <= m && y <= m then
            at_aux x y hg (n+1)
          else if x > m && y <= m then
            at_aux (x-m) y hd (n+1)
          else if x <= m && y > m then
            at_aux x (y-m) bg (n+1)
          else 
            at_aux (x-m) (y-m) bd (n+1)
      in 
        at_aux x y q 1
    
    let remove x y q =
      let rec remove_aux x y q n =
        match q with
        | Empty -> Empty
        | Leaf _ -> Empty  (* Remove the leaf at the specified coordinates *)
        | Node (hg, hd, bg, bd) ->
          let n_max = log2 Screen.length in
          let m = pow 2 (n_max - n)  in
          if x <= m && y <= m then
            let new_hg = remove_aux x y hg (n+1) in
            if is_empty new_hg && is_empty hd && is_empty bg && is_empty bd then
              Empty
            else
              Node (new_hg, hd, bg, bd)
          else if x > m && y <= m then
            let new_hd = remove_aux (x - m) y hd (n+1) in
            if is_empty hg && is_empty new_hd && is_empty bg && is_empty bd then
              Empty
            else
              Node (hg, new_hd, bg, bd)
          else if x <= m && y > m then
            let new_bg = remove_aux x (y - m) bg (n+1) in
            if is_empty hg && is_empty hd && is_empty new_bg && is_empty bd then
              Empty
            else
              Node (hg, hd, new_bg, bd)
          else
            let new_bd = remove_aux (x - m) (y - m) bd (n+1) in
            if is_empty hg && is_empty hd && is_empty bg && is_empty new_bd then
              Empty
            else
              Node (hg, hd, bg, new_bd)
      and is_empty = function
        | Empty -> true
        | _ -> false
      in remove_aux x y q 1 
   
    let rec print_n_space n =
      if n = 0 then print_string ""
      else 
        (print_string " "; print_n_space (n-1))

    let print_int_qt q =
      let rec aux q n =
        match q with  
        | Empty -> print_n_space n; print_endline "E"
        | Leaf a -> print_n_space n; print_endline (string_of_int a)
        | Node (hg, hd, bg, bd) -> (
          print_n_space n; print_endline "[";
          aux hg (n+1);
          print_n_space n; print_endline ",";
          aux hd (n+1);
          print_n_space n; print_endline ",";
          aux bg (n+1);
          print_n_space n; print_endline ",";
          aux bd (n+1);
          print_n_space n; print_endline "]"
        )
      in 
        aux q 0
end

(* *********** TESTS *********** *)
(* TODO: Completer les tests *)

(* We test on a 16 by 16 grid *)
module MyGrid : Square_2D_Space =
struct
  let length = 16
end

(* We instanciate the Fonctor for the 16x16 grid *)
module QT = Quadtree2D(MyGrid)
open QT

let q1 = insert 1 1 0 Empty

let%test _ = remove 1 1 q1 = Empty
let%test _ = remove 1 2 (insert 1 2 1 q1) = q1 
let%test _ = remove 1 1 Empty = Empty
let%test _ = at 1 1 q1 = Some 0
let%test _ = at 2 1 q1 = None
let%test _ = at 1 2 q1 = None

(* TODO: Corriger ce comportement ! *) let%test _ = insert (-1) 1 0 Empty = insert 2 2 0 Empty
