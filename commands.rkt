#lang racket/base
(require racket/gui
         racket/draw
         (for-syntax syntax/parse
                     racket/base))
(provide (all-defined-out))

(require "world.rkt")

(define (logo-set-position x y)
  (match-define (world turt context _) (current-world))
  (match-define (turtle old-x old-y ang pen-down) turt)
  (set-turtle-x! turt (scale-to-width  x))
  (set-turtle-y! turt (scale-to-height y))
  (when pen-down
    (send context draw-line old-x old-y x y)))

(define (logo-set-angle deg)
  (match-define (world turt _ _) (current-world))
  (define rads (degrees->radians deg))
  (set-turtle-angle! turt rads))

(define (logo-forward n)
  (match-define (world turt _ _) (current-world))
  (match-define (turtle old-x old-y ang pen-down) turt)
  (define new-x (scale-to-width  (+ old-x (* (cos ang) n))))
  (define new-y (scale-to-height (+ old-y (* (sin ang) n))))
  (logo-set-position new-x new-y))

(define (logo-back n)
  (logo-forward (- n)))

(define (logo-right deg)
  (match-define (world turt _ _) (current-world))
  (define rads (degrees->radians deg))
  (set-turtle-angle! turt (+ (turtle-angle turt) rads)))

(define (logo-left deg)
  (logo-right (- deg)))

(define (logo-pen-up)
  (match-define (world turt _ _) (current-world))
  (set-turtle-pen-down! turt #f))

(define (logo-pen-down)
  (match-define (world turt _ _) (current-world))
  (set-turtle-pen-down! turt #t))

(define (logo-clear)
  (match-define (world _ context _) (current-world))
  (send context clear))

(define (logo-home)
  (define-values (starting-x starting-y starting-angle) (turtle-starting-position))
  (logo-set-position starting-x starting-y)
  (logo-set-angle starting-angle))

(define (logo-print output [color "black"])
  (match-define (world _ _ text) (current-world))
  (define style-delta (make-object style-delta%
                                   'change-normal-color))
  (send style-delta set-delta-foreground color)
  (send text change-style style-delta)
  (send text insert (format "~a~%" output))
  (send style-delta set-delta-foreground "black")
  (send text change-style style-delta))

(define (logo-error output)
  (logo-print (format "Error: ~a" output) "red"))

(define (logo-random n)
  (random 0 (add1 n)))

(define (logo-expr x)
  x)

(define (logo-sum n [op #f] [m #f])
  (case op
    [("+") (+ n m)]
    [("-") (- n m)]
    [else n]))

(define (logo-negative n [m #f])
  (case n
    [("-") (- m)]
    [else n]))

(define (logo-product n [op #f] [m #f])
  (case op
    [("*") (* n m)]
    [("/") (/ n m)]
    [else n]))

(define (logo-max-width)
  (world-max-width))
(define (logo-max-height)
  (world-max-height))

(define (logo-not x [y #f])
  (case x
    [("not") (not y)]
    [else x]))

(define (logo-cond x op y)
  (case op
    [("=") (= x y)]
    [("<") (< x y)]
    [(">") (> x y)]
    [("<=") (<= x y)]
    [(">=") (>= x y)]
    [("!=") (not (= x y))]))

(define-syntax (logo-if stx)
  (syntax-parse stx
    #:datum-literals (logo-program)
    [(_ expr (logo-program statements ...))
     #'(when expr
         statements ...)]
    [(_ expr (logo-program true-statements ...) (logo-program false-statements ...))
     #'(cond [expr true-statements ...]
             [else false-statements ...])]))

(define-syntax (logo-to stx)
  (syntax-parse stx
    #:datum-literals (logo-program)
    [(_ id args ... (logo-program commands ...))
     (if (identifier-binding #'id)
         #'(set! id
                 (lambda (args ...)
                   commands ...))
         #'(define id (lambda (args ...)
                        commands ...)))]))

(struct stop-signal (value))

(define (logo-stop)
  (raise (stop-signal (void))))

(define (logo-output x)
  (raise (stop-signal x)))

(define (logo-set-pen-color r [g #f] [b #f])
  (match-define (world _ context _) (current-world))
  (define color (if g
                    (make-color r g b)
                    r))
  (define old-pen (send context get-pen))
  (define new-pen (send the-pen-list find-or-create-pen
                        color
                        (send old-pen get-width)
                        (send old-pen get-style)
                        (send old-pen get-cap)
                        (send old-pen get-join)))
  (send context set-pen new-pen))

(define (logo-set-pen-style style)
  (define style-list (list 'solid 'dot 'long-dash 'short-dash 'dot-dash))
  (define new-style (cond [(and (number? style)
                                (not (negative? style))
                                (< style (length style-list)))
                           (list-ref style-list style)]
                          [(and (string? style)
                                (member (string->symbol style) style-list))
                           (string->symbol style)]
                          [else (logo-error (format "Unknown style '~a'. Known styles are 'solid', 'dot', 'long-dash', 'short-dash' and 'dot-dash'." style))
                                #f]))
  (when new-style
    (match-define (world _ context _) (current-world))
    (define old-pen (send context get-pen))
    (define new-pen (send the-pen-list find-or-create-pen
                          (send old-pen get-color)
                          (send old-pen get-width)
                          new-style
                          (send old-pen get-cap)
                          (send old-pen get-join)))
    (send context set-pen new-pen)))

(define-syntax (logo-command stx)
  (syntax-parse stx
    [(_ id args ...)
     #:with arity       (length (syntax->list #'(args ...)))
     #:with line-number (syntax-line #'id)
     (if (identifier-binding #'id)
         #`(cond [(arity-includes? (procedure-arity id) 'arity)
                  (with-handlers ([stop-signal? (lambda (sig)
                                                  (stop-signal-value sig))])
                    (id args ...))]
                 [else
                  (logo-error (format "Too ~a arguments given to ~a at line ~a."
                                      (if (> 'arity (procedure-arity id))
                                          "many"
                                          "few")
                                      'id 'line-number))])
         #'(logo-error (format "I don't know how to ~a." 'id)))]))

(define (logo-comment com)
  (void))

(define-syntax (logo-repeat stx)
  (syntax-parse stx
    [(_ n statements ...)
     #'(let loop ((i n))
         (when (> i 0)
           statements ...
           (loop (- i 1))))]))

(define-syntax (logo-program stx)
  (syntax-parse stx
    [(_ statements ... last-statement)
     #'(begin
         statements ...
         (define result last-statement)
         (save-logo-state)
         (draw-logo-canvas)
         result)]
    [(_) #'(void)]))
