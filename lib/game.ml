type etat_raquette = float * bool 
type etat_balle = Balle.composantes_balle * Flux balle * raquette * briques
type etat_briques = Briques.brique list

type etat_jeu = etat_raquette * etat_balle * etat_briques 
type jeu = Jeu of etat_jeu * etat_jeu Iterator.Flux.t


let init_jeu = 
  Config.Init_pos.(
    Jeu (((racket_x, false), ((ball_x, ball_y),(ball_vx, ball_vy)), Briques.init), 
    Iterator.Flux.union3 Input.mouse (Balle.get_flux ((ball_x, ball_y),(ball_vx, ball_vy))) (Iterator.Flux.constant Briques.init))
  )
  
let next_jeu (Jeu (etat, flux)) =
  match Iterator.Flux.uncons flux with
  | None -> Jeu (etat, flux)
  | Some (new_etat, flux') -> Jeu (new_etat, flux')

(* 
let init_state =
  Config.Init_pos.(
  let pos_initiale = ((ball_x, ball_y),(ball_vx, ball_vy)) in
    Racket (racket_x, Input.mouse),
    Ball (pos_initiale, Balle.get_flux pos_initiale),
    Bricks (Briques.init, Iterator.Flux.constant Briques.init)
  )

let next_state (Racket (pos, rf), Ball (balle_state, bf), Bricks (bs, bsf)) = 
  (match Iterator.Flux.uncons rf with
  | None -> Racket (pos, rf)
  | Some ((new_pos, _click), rf') -> Racket (new_pos, rf')),

  (match Iterator.Flux.uncons bf with
  | None -> Ball (balle_state, bf)
  | Some (new_balle_state, bf') -> Ball (new_balle_state, bf')),

  (match Iterator.Flux.uncons bsf with
  | None -> Bricks (bs, bsf)
  | Some (new_bs, bsf') -> Bricks (new_bs, bsf'))
   *)