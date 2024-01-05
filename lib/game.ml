open Config

type brick = Brick of Graphics.color

type etat_raquette = Racket of float * (float * bool) Iterator.flux
type etat_balle = Ball of Balle.composantes_balle * Balle.composantes_balle Iterator.flux 
type etat_plateau = Board of brick QT.quadtree

type etat_jeu = etat_raquette * etat_balle * etat_plateau

let rec insert_all_bricks bricks board =
  match bricks with
  | [] -> board
  | (i, j)::q -> insert_all_bricks q (QT.insert i j (Brick Graphics.red) board)  

let init_state : etat_jeu = 
  let ball_state = (Config.InitBalle.position, Config.InitBalle.speed) in

  Racket (395., Input.mouse),
  Ball (ball_state, Balle.get_flux ball_state),
  Board (insert_all_bricks (Levels.get_bricks_level 1) Empty)

let next_state (Racket (pos, rf), Ball (balle_state, bf), Board (board)) = 
  (match Iterator.Flux.uncons rf with
  | None -> Racket (pos, rf)
  | Some ((new_pos, _click), rf') -> Racket (new_pos, rf')),

  (match Iterator.Flux.uncons bf with
  | None -> Ball (balle_state, bf)
  | Some (new_balle_state, bf') -> Ball (new_balle_state, bf')),

  Board (Collision.delete_bricks balle_state board)
  