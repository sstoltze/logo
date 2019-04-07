#lang racket/base
(provide read-syntax parse-logo)
(require syntax/strip-context
         "parser.rkt"
         "tokenizer.rkt")

(define (read-syntax path port)
  (define parse-tree (parse path (make-tokenizer port path)))
  (strip-context
   #`(module logo-module logo/expander
       #,parse-tree)))

(define (parse-logo port)
  (parse (make-tokenizer port #f)))
