#lang racket/base
(provide logo-lexer
         make-tokenizer)

(require brag/support)

(define-lex-abbrev reserved-terms (:or "forward" "back" "right" "left" "repeat" "[" "]"
                                       "to" "end" "if" "+" "-" "/" "*" "!=" "="
                                       "<" ">" "(" ")" "stop" "output" "penup" "pendown"
                                       "print" "setposition" "setpos" "setxy"
                                       "random" "not" "clear" "home"))
(define-lex-abbrev digits (:+ (char-set "0123456789")))

(define logo-lexer
  (lexer-srcloc
   ["\n" (token 'NEWLINE lexeme)]
   [whitespace (token lexeme #:skip? #t)]
   [reserved-terms (token lexeme lexeme)]
   [digits (token 'INTEGER (string->number lexeme))]
   [(:+ alphabetic) (token 'ID (string->symbol lexeme))]
   [(:seq ":" (:+ alphabetic)) (token 'ARGUMENT (string->symbol (substring lexeme 1)))]
   [(:or (from/to "\"" "\"") (from/to "\'" "\'"))
    (token 'STRING
           (substring lexeme 1 (sub1 (string-length lexeme))))]))

(define (make-tokenizer ip [path #f])
  (port-count-lines! ip)
  (lexer-file-path path)
  (define (next-token)
    (logo-lexer ip))
  next-token)
