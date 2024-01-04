
type etat_raquette = Raquette of float * (float * bool) Iterator.flux
type etat_balle = Balle of Balle.etat_balle * Balle.etat_balle Iterator.flux 

type etat_jeu = etat_raquette * etat_balle

let init_state = 
  let pos_initiale = ((395.,295.),(50.,-150.)) in
  Raquette (395., Input.mouse),
  Balle (pos_initiale, Balle.B.run pos_initiale )

let next_state (Raquette (pos, rf), Balle (balle_state, bf)) = 
  (match Iterator.Flux.uncons rf with
  | None -> Raquette (pos, rf)
  | Some ((new_pos, _click), rf') -> Raquette (new_pos, rf')),

  (match Iterator.Flux.uncons bf with
  | None -> Balle (balle_state, bf)
  | Some (new_balle_state, bf') -> Balle (new_balle_state, bf'))
  