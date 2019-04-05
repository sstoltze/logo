#lang racket/base
(provide (rename-out [tokenize-only-mod #%module-begin])
         #%app #%top #%top-interaction #%datum)
(require brag/support "tokenizer.rkt"
         (for-syntax syntax/parse
                     racket/base)
         syntax/strip-context)

(define (read-syntax path port)
  (define tokens (apply-tokenizer make-tokenizer port))
  (strip-context
   #`(module logo-token-module logo/tokenize-only
       #,@tokens)))
(module+ reader (provide read-syntax))

(define-syntax (tokenize-only-mod stx)
  (syntax-parse stx
    [(_ token ...)
     #'(#%module-begin
     (list token ...))]))
