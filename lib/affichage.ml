
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

let ball_radius = 1
let raquette_distance_from_bottom = 2
let raquette_width = 50
let raquette_height = 10
let brique_width = int_of_float ((Config.Box.supx -. Config.Box.infx) /. float_of_int (Config.BoardSpace.length))
let brique_height = int_of_float ((Config.Box.supy -. Config.Box.infy) /. float_of_int (Config.BoardSpace.length)) 

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

let dessiner_briques board = 
  let from_grid_coords_to_graphics_coordx x = 
    float_of_int (brique_width * x) +. Config.Box.infx in
  let from_grid_coords_to_graphics_coordy y = 
    float_of_int (brique_height * y) +. Config.Box.infy in

  let rec aux = function
  | [] -> ()
  | (Game.Brick c, (i, j))::q -> 
    dessiner_brique (from_grid_coords_to_graphics_coordx i) (from_grid_coords_to_graphics_coordy j) c;
    aux q 
  
  in
    aux (Config.QT.all_elements_and_coordinates board)

let dessiner_etat etat =
  let rpos, bxpos, bypos, board = 
  match etat with
  | Game.Racket (rpos, _), Game.Ball (((bxpos, bypos), _), _), Game.Board (board) -> rpos, bxpos, bypos, board 
  in
  dessiner_balle bxpos bypos;
  dessiner_raquette rpos;
  dessiner_briques board;
  dessiner_brique Config.Box.infx Config.Box.infy Graphics.blue;
  dessiner_brique (Config.Box.supx-.float_of_int brique_width) (Config.Box.supy-.float_of_int brique_height) Graphics.blue;