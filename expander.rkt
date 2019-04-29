#lang racket/base
(provide (rename-out [logo-module-begin #%module-begin])
         #%top #%app #%top-interaction #%datum
         (all-from-out "world.rkt" "commands.rkt"))

(require "world.rkt" "commands.rkt" "reader.rkt"
         (for-syntax syntax/parse
                     racket/list)
         (except-in racket/gui
                    read-syntax read)
         racket/draw)

(define-syntax (logo-module-begin stx)
  (syntax-parse stx
    [(_ source (logo-program statements ...))
     #:with (id ...) (find-property 'logo-id #'(statements ...))
     #'(#%module-begin
        (module configure-runtime racket/base
          (require logo/reader)
          (current-read-interaction read-syntax))
        (define id
          (lambda args (logo-error (format "I don't know how to ~a." 'id)))) ...
        (current-world (new-world source))
        (sleep/yield 1)
        (logo-program
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
