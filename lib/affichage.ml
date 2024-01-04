
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

let ball_radius = 5
let raquette_distance_from_bottom = 2
let raquette_width = 50
let raquette_height = 10
let brique_width = 40
let brique_height = 5

(* Dessiner la balle à partir de ses coordonées *)
(* x, y : float *)
let dessiner_balle x y =
  Graphics.set_color Graphics.blue;
  Graphics.fill_circle (int_of_float x) (int_of_float y) ball_radius 

(* Dessiner la raquette à partir de sa position en x *)
(* La position en x indique le milieu de la raquette *)
let dessiner_raquette x =
  let true_x = x -. (float_of_int raquette_width) /. 2. in
  Graphics.set_color Graphics.black;
  Graphics.fill_rect (int_of_float true_x) raquette_distance_from_bottom raquette_width raquette_height

(* Dessiner une brique à partir des coordonées de son coin *)
(* en bas a gauche et de sa couleur *)
let dessiner_brique x y color = 
  Graphics.set_color color;
  Graphics.fill_rect (int_of_float x) (int_of_float y) brique_width brique_height

let dessiner_etat etat =
  dessiner_balle 395. 295.;
  dessiner_raquette 395.;
  dessiner_brique 125. 500. Graphics.red;
  dessiner_brique 325. 500. Graphics.green;
  dessiner_brique 525. 500. Graphics.blue; 