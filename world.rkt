#lang racket/base
(provide new-world
         scale-to-width
         scale-to-height
         (struct-out turtle)
         (struct-out world)
         current-world)

(require racket/gui
         racket/draw)

(define *width*  1000)
(define *height* 1000)

(struct turtle (x y ang pen-down) #:mutable)
(struct world  (turt drawing-canvas output-text))

(define current-world (make-parameter #f))

(define (new-world)
  (define t  (turtle (/ *width* 2) (/ *height* 2) 0 #t))
  (define f  (new frame%
                  [label "Logo"]
                  [height (inexact->exact (* 1.1 *height*))]
                  [width  *width*]))
  (define vp (new vertical-pane%
                  [parent f]))
  (define c  (new canvas%
                  [parent vp]
                  [min-height *height*]))
  (define dc (send c get-dc))
  (define p  (new pen%))
  (send dc set-pen p)
  (send f show #t)
  (define te (new text%))
  (define ec (new editor-canvas%
                  [parent vp]
                  [editor te]
                  [min-height (inexact->exact (* 0.1 *height*))]))
  (world t dc te))

(define (scale-to-width x)
  (min (max x 0) *width*))
(define (scale-to-height y)
  (min (max y 0) *height*))
