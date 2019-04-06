#lang racket/base
(provide read-syntax)
(require syntax/strip-context
         "parser.rkt"
         "tokenizer.rkt")

(define (read-syntax path port)
  (define parse-tree (parse path (make-tokenizer port path)))
  (strip-context
   #`(module logo-module logo/expander
       #,parse-tree)))
