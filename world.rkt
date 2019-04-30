#lang racket/base
(require (for-syntax syntax/parse)
         (only-in "reader.rkt" parse-logo)
         brag/support
         racket/contract
         racket/gui
         racket/draw
         racket/runtime-path)
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
                       [turtle-starting-position (-> (values world-size? world-size? number?))]
                       [save-logo-state  (-> void?)]
                       [logo-undo        (-> draw/c)]
                       [draw-logo-canvas (-> draw/c)])
         define/logo lambda/logo ;; Contract?
         )

(define draw/c (or/c boolean? void?))

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
     #'(define define-id
         (lambda/logo (arg ...)
                      statement ...))]))

;; state is turtle, pen, bitmap
(define (save-logo-state)
  (match-define (world turt context _) (current-world))
  (set! current-undo-list (cons (list (struct-copy turtle turt)
                                      (send context get-pen)
                                      (bitmap-context->bytes context))
                                current-undo-list)))
(define (restore-logo-state turtle pen bitmap)
  (match-define (world _ context _) (current-world))
  (restore-turtle turtle)
  (send context set-pen pen)
  (define input-port  (open-input-bytes bitmap))
  (define new-bitmap  (read-bitmap input-port 'png))
  (define new-context (send new-bitmap make-dc))
  (set-world-drawing-context! (current-world) new-context))
(define (bitmap-context->bytes bc)
  (define bitmap (send bc get-bitmap))
  (define output-port (open-output-bytes))
  (send bitmap save-file output-port 'png)
  (close-output-port output-port)
  (get-output-bytes output-port))
(define (restore-turtle turtle)
  (set-world-turtle! (current-world) turtle))
(define (logo-undo)
  (unless (or (empty? current-undo-list)
              (empty? (rest current-undo-list)))
    (match-define (list t p b) (second current-undo-list))
    (restore-logo-state t p b)
    (set! current-undo-list (rest current-undo-list))
    (draw-logo-canvas)))
(define (reset-logo-state turt pen bc dc)
  (set! current-undo-list (list
                           (list (struct-copy turtle turt)
                                 pen
                                 (bitmap-context->bytes bc))))
  (set! current-world-canvas dc))
(define-runtime-path turtle-image-path "./images/logo-turtle.png")
(define turtle-bitmap (read-bitmap turtle-image-path 'png))
(define (draw-logo-canvas)
  (match-define (world t bitmap-context _) (current-world))
  (match-define (turtle x y ang _) t)
  (define bitmap (send bitmap-context get-bitmap))
  (send current-world-canvas clear)
  (send current-world-canvas draw-bitmap bitmap 0 0)
  ;; Draw turtle
  (let ([t-x (- x (/ (send turtle-bitmap get-width)  2))]
        [t-y (- y (/ (send turtle-bitmap get-height) 2))])
    (define rotate-bitmap (make-bitmap (send turtle-bitmap get-width)
                                       (send turtle-bitmap get-height)))
    (define turtle-dc (send rotate-bitmap make-dc))
    (send turtle-dc clear)
    (send turtle-dc set-rotation ang)
    (send turtle-dc draw-bitmap turtle-bitmap 0 0 'opaque)
    ;(send current-world-canvas set-rotation ang)
    (send current-world-canvas draw-bitmap rotate-bitmap t-x t-y 'opaque)
    (send current-world-canvas set-rotation 0)))

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
    (define (print-with-colour colour item)
      (define style-delta (make-object style-delta%
                                       'change-normal-color))
      (send style-delta set-delta-foreground colour)
      (send output change-style style-delta)
      (send output move-position 'end)
      (send output insert (format "~a~%" item))
      (send style-delta set-delta-foreground "black")
      (send output change-style style-delta))
    ;; Get input
    (define program (send input get-text))
    (with-handlers
      ([exn:fail:parsing?
        (lambda (e)
          (let ([src (first (exn:fail:parsing-srclocs e))]
                [msg (exn-message e)])
            (print-with-colour "red" (format "Parsing error at line ~a, column ~a: ~a."
                                             (srcloc-line src)
                                             (srcloc-column src)
                                             msg))))])
      (define code (parse-logo (open-input-string program)))
      (print-with-colour "blue" program)
      ;; Run program
      (eval code)))
  (define (clear-screen button event)
    (send bc erase)
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
  (define (button-undo i e)
    (logo-undo))

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
                             [stretchable-height #f]
                             [paint-callback     (lambda (c dc) (draw-logo-canvas))]))
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
  (define undo          (new button%
                             [parent clear-h]
                             [label "Undo"]
                             [callback button-undo]))
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
