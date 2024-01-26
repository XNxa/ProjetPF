open Config

let contact_x x dx = 
  let xmin, xmax = Box.infx, Box.supx in
  (x > xmax && dx > 0. || x < xmin && dx < 0.)

let rebond_x x dx = if contact_x x dx then -.dx else dx

let contact_y y dy = 
  let ymin, ymax = Box.infy, Box.supy in
  (y > ymax && dy > 0. || y < ymin && dy < 0.)

let rebond_y y dy = if contact_y y dy then -.dy else dy 

let rebond_murs ((pos1, pos2), (vit1, vit2)) =
   ((pos1,pos2), ((rebond_x pos1 vit1),(rebond_y pos2 vit2)))

let contact_murs ((x,y), (dx, dy)) =
  contact_x x dx || contact_y y dy

let collisionCarreCarre x1 y1 w1 h1 x2 y2 w2 h2 =
  not ((x2 >= x1 +. w1) || (x2 +. w2 <= x1) || (y2 >= y1 +. h1) || (y2 +. h2 <= y1))

(* Teste la collision entre un point et un cercle : 
  on compare la distance entre le centre du cercle et le point
  avec le rayon du cercle.
  Param x1 : abscisse du centre du cercle  
  Param y1 : ordonnée du centre du cercle 
  Param x2 : abscisse du point
  Param y2 : ordonnée du point 
*)
let collisionPointCercle x1 y1 x2 y2 r =
  let dx = x1 -. x2 in
  let dy = y1 -. y2 in
  let dcarre = dx *. dx +. dy *. dy in
  dcarre <= r*.r 
  
let collisionPointCarre xp yp x y w h =
  (xp >= x) && (xp <= x +. w) && (yp >= y) && (yp <= y +. h)

let projectionSurSegment cx cy ax ay bx by =
  let acx = cx -. ax in
  let acy = cy -. ay in
  let abx = bx -. ax in
  let aby = by -. ay in
  let bcx = cx -. bx in
  let bcy = cy -. by in
  let s1 = (acx *. abx) +. (acy *. aby) in
  let s2 = (bcx *. abx) +. (bcy *. aby) in
  (s1 *. s2 < 0.) 
  
let collisionCircleAABB (xballe, yballe) (xbrick, ybrick) radius width height = 
  if not (collisionCarreCarre (xballe-.radius) (yballe-.radius) (radius*.2.) (radius*.2.) xbrick ybrick width height) 
    then false
  else 
    if   (collisionPointCercle xbrick ybrick xballe yballe radius)  
      || (collisionPointCercle xbrick (ybrick +. height)  xballe yballe radius)  
      || (collisionPointCercle (xbrick +. width)  ybrick  xballe yballe radius)  
      || (collisionPointCercle (xbrick +. width)  (ybrick +. height) xballe yballe radius) then 
      true
    else 
      if collisionPointCarre xballe yballe xbrick ybrick width height then 
        true
      else 
        (projectionSurSegment xballe yballe xbrick ybrick xbrick (ybrick +. height)) 
        ||
        (projectionSurSegment xballe yballe xbrick ybrick (xbrick +. width) ybrick)

let collisionBalleBrique (xballe, yballe) (xbrick, ybrick) =
  let radius = float_of_int Config.Ball.radius in
  let width = float_of_int Config.Brick.width in
  let height = float_of_int Config.Brick.height in

  collisionCircleAABB (xballe, yballe) (xbrick, ybrick) radius width height
  
(* Verifier le contact de la balle avec la raquette.                  *)
(* Param bx, by : (float * float) : coordonnées du centre de la balle *)
(* Param rpos : float : abscisse du centre de la raquette             *)
(* Résultat : true si il y a contact entre la balle et la raquette    *)
let collisionBalleRaquette (xballe, yballe) xraquette =
  let radius = float_of_int Config.Ball.radius in
  let width = float_of_int Config.Racket.width in
  let height = float_of_int Config.Racket.height in

  collisionCircleAABB (xballe, yballe) (xraquette -. width/.2., float_of_int Config.Racket.distance_from_bottom) radius width height

(* Vérifier le contact avec les briques ou avec la raquette, ou avec un mur. *)
let rec contact (((bx, by), (bdx, bdy)), (rpos, _), list_briques) =
  match list_briques with 
  | [] -> collisionBalleRaquette (bx, by) rpos || contact_murs ((bx, by), (bdx, bdy))
  | (brx, bry)::q -> collisionBalleBrique (bx, by) (brx, bry) || (contact (((bx, by), (bdx, bdy)), (rpos, false), q))


let rec supprime_brique list_brick bx by =
  match list_brick with
  | [] -> []
  | (brx, bry)::q -> if collisionBalleBrique (bx, by) (brx, bry) then q else (brx, bry)::(supprime_brique q bx by)


let rebond (((bx, by), (bdx, bdy)), (rpos, b), list_briques) = 
  if contact_murs ((bx, by), (bdx, bdy)) then 
    (rebond_murs ((bx, by), (bdx, bdy))), (rpos, b), list_briques
  else if collisionBalleRaquette (bx, by) rpos then
      let diff = (bx -. rpos)/.(rpos/.2.) in
        if diff < 0. then
          ((bx, by+.1.), (-.Float.abs(bdx+.diff*.bdx*.0.2), -.bdy+.Config.Acceleration.racket)), (rpos, b), list_briques
        else
          ((bx, by+.1.), (Float.abs(bdx+.diff*.bdx*.0.2), -.bdy+.Config.Acceleration.racket)), (rpos, b), list_briques
    else
      ((bx, by), (bdx, -.bdy)), (rpos, b), (supprime_brique list_briques bx by)



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

module TestCollisionCarreCarre = struct

  (* CollisionCarreCarre  x1 y1 w1 h1 x2 y2 w2 h2*)

  (* Cas x2 = x1 + w1*)
  let%test _ = collisionCarreCarre 2. 0. 1. 2. 3. 0. 1. 1. = false

  (* Cas x2 > x1 + w1*)
  let%test _ = collisionCarreCarre 2. 0. 1. 2. 4. 0. 1. 1. = false

  (* Cas x2 + w2 <= x1 *)
  let%test _ = collisionCarreCarre 5. 0. 1. 2. 3. 2. 1. 1. = false

  (* Cas x2 + w2 < x1 *)
  let%test _ = collisionCarreCarre 6. 0. 1. 2. 3. 2. 1. 1. = false

  (* Cas y2 = y1 + h1*)
  let%test _ = collisionCarreCarre 0. 1. 1. 2. 3. 3. 1. 1. = false

  (* Cas y2 > y1 + h1*)
  let%test _ = collisionCarreCarre 0. 1. 1. 2. 3. 4. 1. 1. = false

  (* Cas y2 + h2 <= y1*)
  let%test _ = collisionCarreCarre 0. 2. 1. 2. 3. 1. 1. 1. = false

  (* Cas y2 + h2 < y1*)
  let%test _ = collisionCarreCarre 0. 3. 1. 2. 3. 1. 1. 1. = false

end