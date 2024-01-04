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