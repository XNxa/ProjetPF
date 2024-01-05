open Config

(* Fonction qui indique si la balle est en contact avec un mur *)
val contact : ((float * float) * (float * float)) -> bool

(* Fonction qui met à jour un état si un contact à lieu *)
val rebond : ((float * float) * (float * float)) -> ((float * float) * (float * float))

val delete_bricks : (float * float) * (float * float) -> 'b QT.quadtree -> 'b QT.quadtree