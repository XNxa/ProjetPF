
module type LevelType =
sig
  val get_bricklist : (float * float) list
end

module Level_1 : LevelType =
struct
  
  let get_bricklist =
    let rec aux i j acc b =
      let brick_x = i *. float_of_int Config.Brick.width in
      let brick_y = (j +. 1.) *. float_of_int Config.Brick.height in
      let new_acc = if int_of_float (i+.j) mod 2 = 0 then (brick_x, brick_y) :: acc else acc in
      if brick_x +. float_of_int Config.Brick.width > Config.Box.supx then
        aux 0. (j +. 1.) new_acc (not b)
      else if brick_y > Config.Box.supy then
        new_acc
      else
        aux (i +. 1.) j new_acc (not b)
    in
  aux 0. (Float.round (Config.Box.supy /. float_of_int Config.Brick.height /. 2. *. 1.)) [] true

end

module Level_2 : LevelType = struct 
  let get_bricklist = [(400., 400.)]
end

let get_bricks_level n =
  match n with 
  | 1 -> Level_1.get_bricklist
  | 2 -> Level_2.get_bricklist
  | _ -> failwith ("Level " ^ (string_of_int n) ^ " undefined")