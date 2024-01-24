
(* Fonction qui indique si la balle est en contact avec un mur *)
val contact : ((float * float) * (float * float)) -> bool

(* Fonction qui met à jour un état si un contact à lieu *)
val rebond : ((float * float) * (float * float)) -> ((float * float) * (float * float))

(* Fonction qui indique si la balle est en contact avec la raquette *)
(* arg1 : pos balle                                                 *)
(* arg2 : pos brique                                                *)
(* return true if collide false otherwise                           *)
val collisionBalleBrique : (float * float) -> (float * float) -> bool