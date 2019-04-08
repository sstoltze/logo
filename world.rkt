#lang racket/base
(require (only-in "reader.rkt" parse-logo)
         racket/contract
         racket/gui
         racket/draw)
(provide (contract-out [scale-to-width   (-> world-size? world-size?)]
                       [scale-to-height  (-> world-size? world-size?)]
                       [world-max-width  (-> world-size?)]
                       [world-max-height (-> world-size?)]
                       [struct turtle    [(x        world-size?)
                                          (y        world-size?)
                                          (ang      number?)
                                          (pen-down boolean?)]]
                       [struct world     [(turt           turtle?)
                                          (drawing-canvas (is-a?/c dc<%>))
                                          (output-text    (is-a?/c text%))]]
                       [current-world    (parameter/c (or/c #f world?))]
                       [new-world        (-> world?)]
                       [turtle-starting-position (-> (values world-size? world-size? number?))]))

(define *width*  800)
(define *height* 800)

(struct turtle (x y ang pen-down) #:mutable)
(struct world  (turt drawing-canvas output-text))

(define current-world (make-parameter #f))

(define (world-size? s)
  (and (number? s) (positive? s)))

(define (scale-to-width x)
  (min (max x 0) *width*))
(define (scale-to-height y)
  (min (max y 0) *height*))
(define (world-max-width)
  *width*)
(define (world-max-height)
  *height*)
(define (turtle-starting-position)
  (define starting-x-position (/ (world-max-width)  2))
  (define starting-y-position (/ (world-max-height) 2))
  (define starting-angle-in-degrees 0)
  (values starting-x-position starting-y-position starting-angle-in-degrees))

(define (new-world)
  ;; Button callbacks
  (define (submit-program button event)
    ;; Get input
    (define program (send input get-text))
    (define code    (parse-logo (open-input-string program)))
    ;; Submit program to log with color
    (define style-delta (make-object style-delta%
                                     'change-normal-color))
    (send style-delta set-delta-foreground "blue")
    (send output change-style style-delta)
    (send output insert (format "~a~%" program))
    (send style-delta set-delta-foreground "black")
    (send output change-style style-delta)
    ;; Run program
    (eval code))
  (define (clear-screen button event)
    (send dc erase))
  (define (reset-turtle button event)
    (define-values (starting-x starting-y starting-angle) (turtle-starting-position))
    (set-turtle-x!   turt starting-x)
    (set-turtle-y!   turt starting-y)
    (set-turtle-ang! turt (degrees->radians starting-angle)))

  ;; Turtle
  (define-values (starting-x starting-y starting-angle) (turtle-starting-position))
  (define turt   (turtle starting-x starting-y starting-angle #t))

  ;; GUI
  (define frame         (new frame%
                             [label "Logo"]
                             [height (inexact->exact (* 1.2 *height*))]
                             [width  *width*]))
  (define vp            (new vertical-pane%
                             [parent frame]))
  (define canvas        (new canvas%
                             [parent vp]
                             [min-height *height*]))
  (define dc            (send canvas get-dc))
  (define pen           (new pen%))
  (send dc set-pen pen)
  (send frame show #t)
  ;; Bottom area
  (define hp            (new horizontal-pane%
                             [parent vp]))
  ;; Input
  (define input-v       (new vertical-pane%
                             [parent hp]))
  (define input-h       (new horizontal-pane%
                             [parent input-v]
                             [alignment '(left center)]))
  (define label-h       (new horizontal-pane%
                             [parent input-h]
                             [alignment '(left center)]))
  (define input-label   (new message%
                             [parent label-h]
                             [label "Input"]))
  (define input         (new text%))
  (define input-canvas  (new editor-canvas%
                             [parent input-v]
                             [editor input]
                             [min-height (inexact->exact (* 0.2 *height*))]))
  ;; Button-row
  (define button-h      (new horizontal-pane%
                             [parent input-v]))
  (define reset-h       (new horizontal-pane%
                             [parent button-h]
                             [alignment '(left center)]))
  (define reset         (new button%
                             [parent reset-h]
                             [label "Reset turtle"]
                             [callback reset-turtle]))
  (define clear-h       (new horizontal-pane%
                             [parent button-h]
                             [alignment '(center center)]))
  (define clear         (new button%
                             [parent clear-h]
                             [label "Clear screen"]
                             [callback clear-screen]))
  (define submit-h      (new horizontal-pane%
                             [parent button-h]
                             [alignment '(right center)]))
  (define submit        (new button%
                             [parent button-h]
                             [label "Submit"]
                             [callback submit-program]))
  ;; Log
  (define output-v      (new vertical-pane%
                             [parent hp]
                             [alignment  '(left center)]))
  (define output-label  (new message%
                             [parent output-v]
                             [label "Log"]))
  (define output        (new text%))
  (define output-canvas (new editor-canvas%
                             [parent output-v]
                             [editor output]
                             [min-height (inexact->exact (* 0.2 *height*))]))
  (world turt dc output))
