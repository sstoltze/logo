#lang logo

to square :length
repeat 4 [forward :length right 90]
end

to flower :size
repeat 36 [right 10 square :size]
end

flower
flower 100
flower 100 20

square 300

to square :size
repeat 3 [forward :size right 120]
end

square 200
