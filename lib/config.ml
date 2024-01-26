module Init = struct
  let dt = 1. /. 60. (* 60 Hz *)
end

module Box = struct
  let marge = 10.
  let infx = 10.
  let infy = 10.
  let supx = 790.
  let supy = 590.
end

module Init_pos = struct
  let racket_x = 495.
  let ball_x = 395.
  let ball_y = 295.
  let ball_vx = 250.
  let ball_vy = -150.
end


module Brick = struct
  let width = 100
  let height = 50
end

module Racket = struct
  let width = 150
  let height = 10
  let distance_from_bottom = 20
end

module Ball = struct
  let radius = 5
end

module Acceleration = struct
  let racket = 1.
end