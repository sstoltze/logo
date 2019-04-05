#lang logo
to spiral :size :angle
if :size > 200 [ stop ]
forward :size
right :angle
spiral :size + 2 :angle
end

spiral 0 91
