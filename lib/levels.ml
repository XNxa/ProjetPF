
module Level_1 = struct
  let taille = 512
  let brick_width = 32
  let brick_height = 16
  
  (* Ajoute les coordonées des cases qui on la somme des indices impaires a une liste *)
  let bricks_pos epaisseur =
    let rec aux i j acc =
      let new_acc = if (i + j) mod 2 = 0 then (i, j) :: acc else acc in
      if j <= taille then
        if i < taille then
          aux (i+brick_height) j new_acc
        else
          aux 1 (j+brick_width) new_acc
      else
        acc
    in
      aux 1 (taille-epaisseur+1) []
end

let brickpos n =
  match n with
  | 1 -> Level_1.bricks_pos 50
  | 2 -> Level_1.bricks_pos 3
  | _ -> Level_1.bricks_pos 4

      
      
