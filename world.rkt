#lang racket/base
(require (for-syntax syntax/parse)
         (only-in "reader.rkt" parse-logo)
         racket/contract
         racket/gui
         racket/draw)
(provide (contract-out [scale-to-width   (-> number? world-size?)]
                       [scale-to-height  (-> number? world-size?)]
                       [world-max-width  (-> world-size?)]
                       [world-max-height (-> world-size?)]
                       [struct turtle    [(x        world-size?)
                                          (y        world-size?)
                                          (angle    number?)
                                          (pen-down boolean?)]]
                       [struct world     [(turtle          turtle?)
                                          (drawing-context (is-a?/c bitmap-dc%))
                                          (output-text     (is-a?/c text%))]]
                       [current-world    (parameter/c (or/c #f world?))]
                       [new-world        (->* () ((or/c #f path?)) world?)]
                       [turtle-starting-position (-> (values world-size? world-size? number?))])
         define/logo lambda/logo ;; Contract?
         save-logo-state draw-logo-canvas restore-logo-state logo-undo) ;; Add contracts

(define *width*  800)
(define *height* 800)

(struct turtle (x y angle pen-down) #:mutable)
(struct world  (turtle drawing-context output-text) #:mutable)

(define current-world        (make-parameter #f))
(define current-world-canvas #f)
(define current-undo-list    empty)

(define-syntax (lambda/logo stx)
  (syntax-parse stx
    [(_ (define-id ...) statement ... last-statement)
     #'(lambda (define-id ...)
           statement ...
           (define result last-statement)
           (save-logo-state)
           (draw-logo-canvas)
           result)]))

(define-syntax (define/logo stx)
  (syntax-parse stx
    [(_ (define-id arg ...) statement ... )
     #'(define define-id (lambda/logo (arg ...) statement ...))]))

;; state is turtle, pen, bitmap
(define (save-logo-state)
  (match-define (world turt context _) (current-world))
  (set! current-undo-list (cons (list (struct-copy turtle turt)
                                      (send context get-pen)
                                      (bitmap-context->bytes context))
                                current-undo-list)))
(define/logo (restore-logo-state turtle pen bitmap)
  (match-define (world _ context _) (current-world))
  (restore-turtle turtle)
  (send context set-pen pen)
  (define input-port  (open-input-bytes bitmap))
  (define new-bitmap  (read-bitmap input-port 'png))
  (define new-context (send new-bitmap make-dc))
  (set-world-drawing-context! new-context))
(define (bitmap-context->bytes bc)
  (define bitmap (send bc get-bitmap))
  (define output-port (open-output-bytes))
  (send bitmap save-file output-port 'png)
  (close-output-port output-port)
  (get-output-bytes output-port))
(define (restore-turtle turtle)
  (set-world-turtle! turtle)
  ;; (match-define (turtle new-x new-y new-angle pen-down) turt)
  ;; (define t (world-turtle (current-world)))
  ;; (set-turtle-x!        t new-x)
  ;; (set-turtle-y!        t new-y)
  ;; (set-turtle-angle!    t new-angle)
  ;; (set-turtle-pen-down! t pen-down)
  )
(define (logo-undo)
  (void))
(define (reset-logo-state turt pen bc dc)
  (set! current-undo-list (list
                           (list (struct-copy turtle turt)
                                 pen
                                 (bitmap-context->bytes bc))))
  (set! current-world-canvas dc))
;; Also draw turtle
(define (draw-logo-canvas)
  (define bitmap-context (world-drawing-context (current-world)))
  (define bitmap (send bitmap-context get-bitmap))
  (send current-world-canvas clear)
  (send current-world-canvas draw-bitmap bitmap 0 0)
  ;; Draw turtle ...
  )

;; (define o (open-output-bytes))
;; (send bitmap save-file o 'png)
;; (close-output-port o)
;; (define l (read-bitmap (open-input-bytes (get-output-bytes o)) 'png))


(define (world-size? s)
  (and (number? s) (>=/c 0)))

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

;; Setup current-world-canvas and current-undo-list
(define (new-world [source-file #f])
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
    (send output move-position 'end)
    (send output insert (format "~a~%" program))
    (send style-delta set-delta-foreground "black")
    (send output change-style style-delta)
    ;; Run program
    (eval code))
  (define (clear-screen button event)
    (send dc erase))
  (define (reset-turtle button event)
    (define-values (starting-x starting-y starting-angle) (turtle-starting-position))
    (set-turtle-x!     turt starting-x)
    (set-turtle-y!     turt starting-y)
    (set-turtle-angle! turt (degrees->radians starting-angle)))
  (define (save-file i event)
    ;; Append '#lang logo' if not already there?
    (send input save-file #f 'text))
  (define (open-file i event)
    ;; Remove '#lang logo' if first line?
    (define path (get-file #f frame))
    (when path
      (send input load-file path 'text)))

  ;; Turtle
  (define-values (starting-x starting-y starting-angle) (turtle-starting-position))
  (define turt   (turtle starting-x starting-y starting-angle #t))

  ;; GUI
  (define frame         (new frame%
                             [label  "Logo"]
                             [height (inexact->exact (* 1.2 *height*))]
                             [width  *width*]))
  ;; Menu-bar
  (define mb            (new menu-bar%
                             [parent frame]))
  (define m-file        (new menu%
                             [label "File"]
                             [parent mb]))
  (define m-edit        (new menu%
                             [label "Edit"]
                             [parent mb]))
  (define m-font        (new menu%
                             [label "Font"]
                             [parent mb]))
  (define mi-open       (new menu-item%
                             [label "Open"]
                             [parent m-file]
                             [callback open-file]
                             [shortcut #\o]
                             [shortcut-prefix '(ctl)]))
  (define mi-save       (new menu-item%
                             [label "Save"]
                             [parent m-file]
                             [callback save-file]
                             [shortcut #\s]
                             [shortcut-prefix '(ctl)]))
  (append-editor-operation-menu-items m-edit #f)
  (append-editor-font-menu-items m-font)
  ;; Main window
  (define vp            (new vertical-pane%
                             [parent frame]))
  ;; Drawing area
  (define canvas        (new canvas%
                             [parent             vp]
                             [min-width          *width*]
                             [min-height         *height*]
                             [stretchable-width  #f]
                             [stretchable-height #f]))
  (define dc            (send canvas get-dc))
  (define b             (make-bitmap *width* *height*))
  (define bc            (send b make-dc))
  (define pen           (new pen%))
  (send bc set-pen pen)
  (send frame show #t)
  ;; Bottom area
  (define hp            (new horizontal-pane%
                             [parent vp]))
  ;; Input
  (define input-box     (new group-box-panel%
                             [parent    hp]
                             [label     "Input"]
                             [min-width (scale *width* 0.5)]))
  (define input-v       (new vertical-pane%
                             [parent input-box]))
  (define input-h       (new horizontal-pane%
                             [parent input-v]
                             [alignment '(left center)]))
  (define input         (new text%))
  (define input-canvas  (new editor-canvas%
                             [parent input-v]
                             [editor input]
                             [min-height (scale *height* 0.2)]))
  (send input set-max-undo-history 100)
  (when source-file
    ;; Remove '#lang logo' if first line?
    (send input load-file source-file 'text))
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
  (define output-box    (new group-box-panel%
                             [parent hp]
                             [label "Log"]
                             [min-width (scale *width* 0.5)]))
  (define output        (new append-only-text%))
  (define output-canvas (new editor-canvas%
                             [parent output-box]
                             [editor output]
                             [min-height (scale *height* 0.2)]))
  (reset-logo-state turt pen bc dc)
  (world turt bc output))

(define (scale size ratio)
  (inexact->exact (* ratio size)))

(define append-only-text%
  (class text%
    (inherit last-position)
    (define/augment (can-insert? s l) (= s (last-position)))
    (define/augment (can-delete? s l) #f)
    (super-new)))
