#lang logo
to factorial :number
if :number = 1 [output 1]
output :number * factorial [:number - 1]
end
print factorial 5
factorial
print factorial 3
forward factorial 10
