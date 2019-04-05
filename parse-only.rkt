#lang racket/base
(module+ reader
  (provide read-syntax))
(provide (rename-out [logo-parse-module #%module-begin])
         #%app #%datum #%top #%top-interaction
         read-syntax)
(require "parser.rkt" "tokenizer.rkt"
         (for-syntax syntax/parse
                     racket/base)
         syntax/strip-context)

(define (read-syntax path port)
  (define parse-tree (parse path (make-tokenizer port path)))
  (strip-context
   #`(module logo-parser-mod logo/parse-only
       #,parse-tree)))

(define-syntax (logo-parse-module stx)
  (syntax-parse stx
    [(_ (logo-program statements ...))
     #'(#%module-begin
        '(parameterize ([current-world (new-world)])
           (sleep/yield 1)
           statements ...))]))
