#lang logo

to square :size
repeat 4 [forward :size right 90]
end

to flower :size
repeat 36 [right 10 square :size]
end

to garden :number :size
repeat :number [penup setposition random 800 random 800 pendown flower :size]
end

garden 20 50
