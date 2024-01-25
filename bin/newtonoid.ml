(* ouvre la bibliotheque de modules definis dans lib/ *)
open Libnewtonoid
open Iterator
open Config
open Affichage

let graphic_format =
  Format.sprintf
    " %dx%d+50+50"
    (int_of_float ((2. *. Box.marge) +. Box.supx -. Box.infx))
    (int_of_float ((2. *. Box.marge) +. Box.supy -. Box.infy))

(* extrait le score courant d'un etat : *)
let score _etat : int = 10 (* failwith "A DEFINIR" *)

let check_empty (Game.Jeu (_, f)) = Flux.uncons f = None 

let draw =
  let rec loop flux_etat last_score =
    match Flux.(uncons flux_etat) with
    | None -> last_score
    | Some (etat, flux_etat') ->
      Graphics.clear_graph ();
      (* DESSIN ETAT *)
      dessiner_etat etat;
      (* FIN DESSIN ETAT *)
      Graphics.synchronize ();
      Unix.sleepf Init.dt;
      loop flux_etat' (last_score + score etat)
  in
  Graphics.open_graph graphic_format;
  Graphics.auto_synchronize false;
  Format.printf "Initialisation du jeu.@\n";
  let etats = 
    Flux.unfold 
    (fun current_state -> 
      let next = Game.next_state current_state in
      if check_empty next then None else Some (current_state, next)) Game.init_state
  in
  let score = loop etats 0 in
  Format.printf "Score final : %d@\n" score;
  Graphics.close_graph ()


let () = draw