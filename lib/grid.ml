open Quadtree

module Ecran : Square_2D_Space = struct let length = 512 end
module Grid = Quadtree2D(Ecran)

let brick_width = 32
let brick_height = 16

let raquette_width = 32
let raquette_height = 8
let raquette_height_min = 1

let inserer_brick qt (px, py) color =
  let rec aux x y qt = 
    if x >= brick_width then
      qt
    else
      if y >= brick_height then 
        aux (x+1) 0 qt 
      else
        aux x (y+1) (Grid.insert (px+x) (py+y) color qt) 
  in 
    aux 0 0 qt

let move_raquette ox nx color grid =
  let rec del x y qt =
    if x >= raquette_width then
      qt
    else 
      if y >= raquette_height then 
        del (x+1) 0 qt
      else
        del x (y+1) (Grid.remove (ox+x) (raquette_height_min+y) qt)
  in
  let rec add x y qt =
    if x >= raquette_width then
      qt
    else 
      if y >= raquette_height then 
        add (x+1) 0 qt
      else
        add x (y+1) (Grid.insert (nx+x) (raquette_height_min+y) color qt)
  in 
    add 0 0 (del 0 0 grid)


let dessin grille ex ey = 
  let convert_x x = int_of_float ((float_of_int x) *. ex /. 512.) in
  let convert_y y = int_of_float ((float_of_int y) *. ey /. 512.) in
  let w = int_of_float (ex /. 512.) in let h = int_of_float (ey /. 512.) in

  let rec aux points = 
    match points with
    | [] -> ()
    | (_, (x, y))::pts -> Graphics.fill_rect (convert_x x) (convert_y y) w h; aux pts
  in 
    aux (Grid.all_elements_and_coordinates grille)