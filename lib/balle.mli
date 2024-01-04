open Iterator

(* Type representant la position et la vitesse de la balle *)
type composantes_balle = (float * float) * (float * float)

(* Obtenir le flux des etats à partir de l'etat initial *)
val get_flux : composantes_balle -> composantes_balle flux