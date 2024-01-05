module Init = struct
  let dt = 1. /. 120. (* 60 Hz *)
end

module Box = struct
  let marge = 10.
  let infx = 10.
  let infy = 10.
  let supx = 790.
  let supy = 590.
end

module InitBalle = struct
  let position = 395.,295.
  let speed = 2.,-100.
end

module BoardSpace : Quadtree.Square_2D_Space = 
struct
  let length = 32
end 

module QT = Quadtree.Quadtree2D(BoardSpace)