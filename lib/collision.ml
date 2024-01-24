open Config


let contact_x x dx = 
  let xmin, xmax = Box.infx, Box.supx in
  (x > xmax && dx > 0. || x < xmin && dx < 0.)

let rebond_x x dx = if contact_x x dx then -.dx else dx

let contact_y y dy = 
  let ymin, ymax = Box.infy, Box.supy in
  (y > ymax && dy > 0. || y < ymin && dy < 0.)

let rebond_y y dy = if contact_y y dy then -.dy else dy 

let rebond ((pos1, pos2), (vit1, vit2)) =
   ((pos1,pos2), ((rebond_x pos1 vit1),(rebond_y pos2 vit2)))

let contact ((x,y), (dx, dy)) =
  contact_x x dx || contact_y y dy

(* 
   bool CollisionCercleAABB(Cercle C1,AABB box1)
{
   AABB boxCercle = GetBoxAutourCercle(C1);  // retourner la bounding box de l'image porteuse, ou calculer la bounding box.
   if (CollisionAABBvsAABB(box1,boxCercle)==0)
      return false;   // premier test
   if (CollisionPointCercle(box1.x,box1.y,C1)==1
    || CollisionPointCercle(box1.x,box1.y+box1.h,C1)==1
    || CollisionPointCercle(box1.x+box1.w,box1.y,C1)==1
    || CollisionPointCercle(box1.x+box1.w,box1.y+box1.h,C1)==1)
      return true;   // deuxieme test
   if (CollisionPointAABB(C1.x,C1.y,box1)==1)
      return true;   // troisieme test
   int projvertical = ProjectionSurSegment(C1.x,C1.y,box1.x,box1.y,box1.x,box1.y+box1.h);
   int projhorizontal = ProjectionSurSegment(C1.x,C1.y,box1.x,box1.y,box1.x+box1.w,box1.y); 
   if (projvertical==1 || projhorizontal==1)
      return true;   // cas E
   return false;  // cas B   
}

int ProjectionSurSegment(int Cx,int Cy,int Ax,int Ay,int Bx,int By)
{
   int ACx = Cx-Ax;
   int ACy = Cy-Ay; 
   int ABx = Bx-Ax;
   int ABy = By-Ay; 
   int BCx = Cx-Bx;
   int BCy = Cy-By; 
   int s1 = (ACx*.ABx) + (ACy*.ABy);
   int s2 = (BCx*.ABx) + (BCy*.ABy); 
   if (s1*.s2>0)
     return 0;
   return 1;
}
*)


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