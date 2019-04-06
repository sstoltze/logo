#lang racket/base
(module+ reader
  (provide read-syntax))
(module configure-runtime racket/base
  (require logo/reader)
  (current-read-interaction read-syntax))
(provide read-syntax)
(require "reader.rkt")
