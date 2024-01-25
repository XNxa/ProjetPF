type etat_raquette = float * bool 
type etat_briques = Briques.brique list
type etat_balle = Balle.composantes_balle

type etat_jeu = etat_balle * etat_raquette * etat_briques 
type jeu = Jeu of etat_jeu * etat_jeu Iterator.Flux.t




let init_state =
  Config.Init_pos.( 
  let pos_initiale = ((ball_x, ball_y),(ball_vx, ball_vy)) in
    Jeu ((pos_initiale, (racket_x, false), Briques.init), Balle.get_flux (pos_initiale, (racket_x, false), Briques.init))
  )


let next_state (Jeu (etat, flux)) =
  Iterator.(
  match Flux.uncons flux with 
  | None -> Jeu (etat, Flux.vide)
  | Some (new_state, qf) -> Jeu (new_state, qf)
  )