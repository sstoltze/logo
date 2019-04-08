#lang logo
to style :index
if :index > 4 [stop]
setpenstyle :index
forward [1+:index]*100
right 90
style :index + 1
end

style 0

setps 10
setps "non-existent"
