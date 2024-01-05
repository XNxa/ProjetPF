open Config

let contact_x x dx = 
  let xmin, xmax = Box.infx, Box.supx in
  (x > xmax && dx > 0. || x < xmin && dx < 0.)

let rebond_x x dx = if contact_x x dx then -.dx else dx

let contact_y y dy = 
  let ymin, ymax = Box.infy, Box.supy in
  (y > ymax && dy > 0. || y < ymin && dy < 0.)

let rebond_y y dy = if contact_y y dy then -.dy else dy 

let rebond ((pos1, pos2), (vit1, vit2)) =
   ((pos1,pos2), ((rebond_x pos1 vit1),(rebond_y pos2 vit2)))

let contact ((x,y), (dx, dy)) =
  contact_x x dx || contact_y y dy


let delete_bricks ((x, y), _) board =
  let width = Box.supx -. Box.infx in 
  let x_int = int_of_float (floor (x -. Box.infx) /. width *. float_of_int (BoardSpace.length )) + 1 in 
  let height = Box.supy -. Box.infy in
  let y_int = int_of_float (floor (y -. Box.infy) /. height *. float_of_int (BoardSpace.length )) + 1 in 

  match QT.at x_int y_int board with  
  | None -> board
  | Some(_) -> QT.remove x_int y_int board

  