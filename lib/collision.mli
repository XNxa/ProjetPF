
(* Fonction qui indique si la balle est en contact avec un mur *)
val contact_murs : ((float * float) * (float * float)) -> bool

(* Fonction qui met à jour un état si un contact à lieu *)
val rebond_murs : ((float * float) * (float * float)) -> ((float * float) * (float * float))

(* Fonction qui indique si la balle est en contact avec la raquette *)
(* arg1 : pos balle                                                 *)
(* arg2 : pos brique                                                *)
(* return true if collide false otherwise                           *)
val collisionBalleBrique : (float * float) -> (float * float) -> bool

(* fonction qui indique si la balle est en contact avec les briques ou avec la raquette, 
ou avec un mur. *)

val contact : ((float * float) * (float * float)) * (float * bool) * ((float * float) list) -> bool
val rebond : ((float * float) * (float * float)) * (float * bool) * ((float * float) list) 
    -> ((float * float) * (float * float)) * (float * bool) * ((float * float) list)