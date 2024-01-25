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
  if (x2 >= x1 +. w1) || (x2 +. w2 <= x1) || (y2 >= y1 +. h1) || (y2 +. h2 <= y1) then 
    false 
  else true

let collisionPointCercle x1 y1 x2 y2 r =
  let dx = x1 -. x2 in
  let dy = y1 -. y2 in
  let dcarre = dx *. dx +. dy *. dy in
  if dcarre <= r*.r then true 
  else false

let collisionPointCarre xp yp x y w h =
  if (x >= xp) && (x <= xp +. w) && (y >= yp) && (y <= yp +. h) then true
  else false

let projectionSurSegment cx cy ax ay bx by =
  let acx = cx -. ax in
  let acy = cy -. ay in
  let abx = bx -. ax in
  let aby = by -. ay in
  let bcx = cx -. bx in
  let bcy = cy -. by in
  let s1 = (acx *. abx) +. (acy *. aby) in
  let s2 = (bcx *. abx) +. (bcy *. aby) in
  if (s1 *. s2 > 0.) then false
  else true

let collisionBalleBrique (xballe, yballe) (xbrick, ybrick) =
  let radius = float_of_int Config.Ball.radius in
  let width = float_of_int Config.Brick.width in
  let height = float_of_int Config.Brick.height in

  if not (collisionCarreCarre xballe yballe radius radius xbrick ybrick width height) 
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
        projectionSurSegment xballe yballe xbrick ybrick xbrick (ybrick +. height) 
        ||
        projectionSurSegment xballe yballe xbrick ybrick (xbrick +. width) ybrick
  


(* Verifier le contact de la balle avec la raquette.                  *)
(* Param bx, by : (float * float) : coordonnées du centre de la balle *)
(* Param rpos : float : abscisse du centre de la raquette             *)
(* Résultat : true si il y a contact entre la balle et la raquette    *)
let contact_raquette (bx, by) rpos = 
  collisionBalleBrique (bx, by) (rpos -. float_of_int Config.Racket.width/.2., float_of_int Racket.distance_from_bottom)

(* Vérifier le contact avec les briques ou avec la raquette, ou avec un mur. *)
let rec contact ((bx, by), (bdx, bdy)) (rpos, _) list_briques =
  match list_briques with 
  | [] -> contact_raquette (bx, by) rpos || contact_murs ((bx, by), (bdx, bdy))
  | (brx, bry)::q -> collisionBalleBrique (bx, by) (brx, bry) || (contact ((bx, by), (bdx, bdy)) (rpos, false) q)


let rebond ((bx, by), (bdx, bdy)) (rpos, b) list_briques = 
  if contact_murs ((bx, by), (bdx, bdy)) then 
    (rebond_murs ((bx, by), (bdx, bdy))), (rpos, b), list_briques
  else if contact_raquette (bx, by) rpos then
      ((bx, by), (bdx, bdy)), (rpos, b), list_briques (* TODO*)
    else (* On un contact avec une brique : la supprimer de la liste des briques TODO *)
      ((bx, by), (bdx, bdy)), (rpos, b), list_briques