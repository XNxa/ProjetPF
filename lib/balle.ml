open Iterator

(* le type des états de la balle de la forme (x, y), (dx, dy) *)
(* i.e. position (x, y) et vitesse (dx, dy)                   *)
type composantes_balle = (float * float) * (float * float)

(* Fonction qui intègre/somme les valeurs successives du flux *)
(* avec un pas de temps dt et une valeur initiale nulle, i.e. *)
(* acc_0 = 0; acc_{i+1} = acc_{i} + dt * flux_{i}             *)
(* paramètres:                                                *)
(* dt : float                                                 *)
(* flux : (float * float) Flux.t                              *)
let integre dt flux =
  (* valeur initiale de l'intégrateur                         *)
  let init = ( 0., 0.) in
  (* fonction auxiliaire de calcul de acc_{i} + dt * flux_{i} *)
  let iter (acc1, acc2) (flux1, flux2) =
    (acc1 +. dt *. flux1, acc2 +. dt *. flux2) in
  (* définition récursive du flux acc                         *)
  let rec acc =
    Tick (lazy (Some (init, Flux.map2 iter acc flux)))
  in acc;;

(* Module du modèle dynamique d'une balle en 2D.               *)
(* A partir d'un état initial, run produit le flux des états   *)
(* successifs de la balle, qui pourra être affiché             *)
module FreeFall(Init : sig val dt:float end) =
struct
  let g = 9.81
  let run ((pos1, pos2), (vit1, vit2)) =
      let acceleration = Flux.constant (0., -.g) in
    
      let vitesse = Flux.map (fun (e1,e2) -> (e1+.vit1, e2+.vit2)) 
              (integre Init.dt acceleration) in
    
      let position = Flux.map (fun (e1, e2) -> (e1+.pos1, e2+.pos2)) 
              (integre Init.dt vitesse) in
    
    Flux.map2 (fun a b -> (a,b)) position vitesse
end

module type Frame =
  sig
    val dt : float
    val box_x : float * float
    val box_y : float * float
end

let rec unless fl cond f =
  Tick (lazy  
    Flux.(
      match uncons fl with
      | None -> None
      | Some (t, q) -> if cond t then Some (t, f t) else Some (t, cons t (unless q cond f))
))

module Bouncing (F: Frame) =
struct
  let cond ((pos1, pos2), (vit1, vit2)) = (Collision.contact_x pos1 vit1) || (Collision.contact_y pos2 vit2)
  module FF = FreeFall (F)
  let rec run etat = unless (FF.run etat) cond (fun etat -> (run (Collision.rebond etat)))
end

module F : Frame =
struct
  let dt = Config.Init.dt
  let box_x = Config.Box.infx, Config.Box.supx
  let box_y = Config.Box.infy, Config.Box.supy
end 

module Export = Bouncing(F)

let get_flux = Export.run