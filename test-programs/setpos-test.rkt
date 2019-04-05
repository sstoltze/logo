#lang logo

to square :length
repeat 4 [forward :length right 90]
end

to flower :size
repeat 36 [right 10 square :size]
end

to garden :size
repeat 36 [ penup setposition random 1000 random 1000 pendown flower :size]
end

garden 20
