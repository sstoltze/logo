#lang racket
(require logo/tokenizer
         brag/support)

(apply-tokenizer-maker make-tokenizer "to test\nforward 100\nend")

(apply-tokenizer-maker make-tokenizer
                       #<<prog
to square
repeat 4 [ forward 100 right 90 ]
end
prog
)
