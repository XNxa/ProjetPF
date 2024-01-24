open Config

(* 
  Note : Dans la librairie Graphics d'OCaml
  les coordonées sont définis depuis le coin 
  bas gauche de l'écran :
  
  y
  ^
  |
  |
  |
  |
  0--------------> x
*)
(* Dessiner la balle à partir de ses coordonées *)
(* x, y : float *)
let dessiner_balle x y =
  Graphics.set_color Graphics.blue;
  Graphics.fill_circle (int_of_float x) (int_of_float y) Ball.radius 

(* Dessiner la raquette à partir de sa position en x *)
(* La position en x indique le milieu de la raquette *)
let dessiner_raquette x =
  let true_x = x -. (float_of_int Racket.width) /. 2. in
  Graphics.set_color Graphics.black;
  Graphics.fill_rect (int_of_float true_x) Racket.distance_from_bottom Racket.width Racket.height

(* Dessiner une brique à partir des coordonées de son coin *)
(* en bas a gauche et de sa couleur *)
let dessiner_brique x y color = 
  Graphics.set_color color;
  Graphics.fill_rect (int_of_float x) (int_of_float y) Brick.width Brick.height

let dessiner_etat etat =
  let rpos, bxpos, bypos = 
  match etat with
  | Game.Racket (rpos, _), Game.Ball (((bxpos, bypos), _), _) -> rpos, bxpos, bypos 
  in
  dessiner_balle bxpos bypos;
  dessiner_raquette rpos;
  dessiner_brique 125. 500. Graphics.red;
  dessiner_brique 325. 500. Graphics.green;
  dessiner_brique 525. 500. Graphics.blue; 