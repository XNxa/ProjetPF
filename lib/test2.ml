module TestCollisionPointCercle = struct
  (* Tester avec le centre du cercle *)
  let%test _ = (collisionPointCercle 1. 1. 1. 1. 1.)
  (* Tester avec un point aux limites *)
  let%test _ = collisionPointCercle 1. 1. 1. 2. 1.
  let%test _ = collisionPointCercle 1. 1. 2. 1. 1.
  let%test _ = collisionPointCercle 1. 1. 0. 1. 1.
  let%test _ = collisionPointCercle 1. 1. 0. 1. 1.
  
  (* Tester avec des points en dehors *)
  let%test _ = not (collisionPointCercle 1. 1. 3. 3. 1.)
  let%test _ = not (collisionPointCercle 1. 1. (-.3.) (-.3.) 1.)
  let%test _ = not (collisionPointCercle 1. 1. 3. (-.3.) 1.)
  let%test _ = not (collisionPointCercle 1. 1. (-.3.) 3. 1.)

  end


(* Teste la collision entre un point et un cercle : 
  on compare la distance entre le centre du cercle et le point
  avec le rayon du cercle.
  Param x1 : abscisse du centre du cercle  
  Param y1 : ordonnée du centre du cercle 
  Param x2 : abscisse du point
  Param y2 : ordonnée du point 
*)