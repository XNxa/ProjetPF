open Collision

module Tests =
struct
  
(* CollisionCarreCarre  x1 y1 w1 h1 x2 y2 w2 h2*)

(* Cas x2 = x1 + w1*)
let%test _ = collisionCarreCarre 2. 0. 1. 2. 3. 0. 1. 1. = false

(* Cas x2 > x1 + w1*)
let%test _ = collisionCarreCarre 2. 0. 1. 2. 4. 0. 1. 1. = false

(* Cas x2 + w2 <= x1 *)
let%test _ = collisionCarreCarre 5. 0. 1. 2. 3. 2. 1. 1. = false

(* Cas x2 + w2 < x1 *)
let%test _ = collisionCarreCarre 6. 0. 1. 2. 3. 2. 1. 1. = false

(* Cas y2 = y1 + h1*)
let%test _ = collisionCarreCarre 0. 1. 1. 2. 3. 3. 1. 1. = false

(* Cas y2 > y1 + h1*)
let%test _ = collisionCarreCarre 0. 1. 1. 2. 3. 4. 1. 1. = false

(* Cas y2 + h2 <= y1*)
let%test _ = collisionCarreCarre 0. 2. 1. 2. 3. 1. 1. 1. = false

(* Cas y2 + h2 < y1*)
let%test _ = collisionCarreCarre 0. 3. 1. 2. 3. 1. 1. 1. = false

end