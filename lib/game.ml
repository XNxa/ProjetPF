open Grid
open Iterator

type position = float * float
type vitesse = float * float
type color = 
  | Rouge | Vert | Jaune | Rose | Noir
type sante = | Entiere | Cassee
type brick = int * int * sante * color

type etat_balle = position * vitesse
type etat_briques = brick list
type etat_grille = color Grid.quadtree
type etat_raquette = Position of int
type score = int 

type etat = 
| Etat of etat_balle * etat_briques * etat_grille * etat_raquette * score

let rec inserer_bricks brickposlist = 
  match brickposlist with
  | [] -> Grid.Empty
  | p::q -> inserer_brick (inserer_bricks q) p Rouge 

let init_state level = 
  let balle = (0., 0.), (0., 0.) in
  let briques = Levels.brickpos level in
  let grille = inserer_bricks briques in
  let raquette = Position 0 in
  let score = 0 in
  (balle, briques, grille, raquette, score)


let next (balle, briques, grille, raquette, score) = 
  let new_raquette = 
    let (x, _) = Graphics.mouse_pos () in Position x

  in 
  (balle,
  briques,
  (move_raquette (match raquette with | Position x -> x) (match new_raquette with | Position x -> x) Noir grille),
  new_raquette,
  score)


  

  (* Modifier la position de la raquette *)
  (* Checker les collision de la balle *)
    (* Changer l'etat de la balle en consequence *)
  (* Mettre à jour la liste des bricks *)