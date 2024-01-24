
module type LevelType =
sig
  val get_bricklist : (float * float) list
end

module Level_1 : LevelType =
struct
  (* let length = 32 *)
  (* let get_bricklist =
    let rec aux i j acc =
      let new_acc = if (i+j) mod 2 = 0 then (i,j)::acc else acc in
      if i > 32 then 
        aux 1 (j-1) new_acc
      else 
        if j >= 1 then 
          aux (i+1) j new_acc
        else
          acc
    in
    aux 1 length [] *)

  let get_bricklist =
    [(400., 450.)]
end


let get_bricks_level n =
  match n with 
  | 1 -> Level_1.get_bricklist
  | _ -> failwith ("Level " ^ (string_of_int n) ^ " undefined")