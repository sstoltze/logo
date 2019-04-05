#lang racket/base
(provide (rename-out [logo-module-begin #%module-begin])
         #%top #%app #%top-interaction #%datum
         (all-from-out "world.rkt" "commands.rkt"))

(require "world.rkt" "commands.rkt"
         (for-syntax syntax/parse
                     racket/list)
         racket/gui
         racket/draw)

(define-syntax (logo-module-begin stx)
  (syntax-parse stx
    [(_ (logo-program statements ...))
     #:with (id ...) (find-property 'logo-id #'(statements ...))
     #'(#%module-begin
        (define id
          (lambda args (logo-error (format "I don't know how to ~a." 'id)))) ...
        (parameterize ([current-world (new-world)])
          (sleep/yield 1)
          statements ...))]))

(begin-for-syntax
  (define (syntax-flatten stx)
    (let* ([stx-unwrapped (syntax-e stx)]
           [maybe-pair (and (pair? stx-unwrapped)
                            (flatten stx-unwrapped))])
      (if maybe-pair
          (append-map syntax-flatten maybe-pair)
          (list stx))))
  (define (unique-ids stxs)
    (remove-duplicates stxs #:key syntax->datum))
  (define (find-property which line-stxs)
    (unique-ids
     (for/list ([stx (in-list (syntax-flatten line-stxs))]
                #:when (syntax-property stx which))
       stx))))
