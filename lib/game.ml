
type etat_raquette = Racket of float * (float * bool) Iterator.flux
type etat_balle = Ball of Balle.composantes_balle * Balle.composantes_balle Iterator.flux 

type etat_jeu = etat_raquette * etat_balle

let init_state =
  Config.Init_pos.( 
  let pos_initiale = ((ball_x, ball_y),(ball_vx, ball_vy)) in
  Racket (racket_x, Input.mouse),
  Ball (pos_initiale, Balle.get_flux pos_initiale))

let next_state (Racket (pos, rf), Ball (balle_state, bf)) = 
  (match Iterator.Flux.uncons rf with
  | None -> Racket (pos, rf)
  | Some ((new_pos, _click), rf') -> Racket (new_pos, rf')),

  (match Iterator.Flux.uncons bf with
  | None -> Ball (balle_state, bf)
  | Some (new_balle_state, bf') -> Ball (new_balle_state, bf'))
  