#lang brag

logo-program    : [logo-line] ((/NEWLINE)* [logo-line])*
@logo-line      : logo-statement
@logo-statement : logo-forward | logo-right | logo-left | logo-repeat | logo-to | logo-if
                | logo-stop | logo-output | logo-pen-up | logo-pen-down | logo-print | logo-back
                | logo-set-position | logo-random
                | logo-command
logo-forward    : /"forward" logo-expr
logo-back       : /"back"    logo-expr
logo-right      : /"right"   logo-expr
logo-left       : /"left"    logo-expr
logo-repeat     : /"repeat"  logo-expr /"[" @logo-program /"]"
logo-to         : /"to" logo-id ([logo-argument])* logo-program /"end"
logo-if         : /"if" logo-not /"[" logo-program /"]" [/"[" logo-program /"]"]
logo-stop       : /"stop"
logo-output     : /"output" logo-expr
logo-pen-up     : /"penup"
logo-pen-down   : /"pendown"
logo-clear      : /"clear"
logo-home       : /"home"
logo-print      : /"print" logo-expr
logo-set-position : /("setposition"|"setpos"|"setxy") logo-expr logo-expr
logo-random     : /"random" logo-expr
logo-command    : logo-id ([logo-expr])*
logo-not        : ["not"] logo-cond
logo-cond       : logo-expr ("="|"<"|">"|"!=") logo-expr
logo-expr       : logo-sum
logo-sum        : [logo-sum ("+"|"-")] logo-product
logo-product    : [logo-product ("*"|"/")] logo-negative
logo-negative   : ["-"] logo-var
@logo-var       : logo-int | logo-string | logo-argument | logo-command | logo-statement
                | /("("|"[") logo-expr /(")"|"]")
@logo-string    : STRING
@logo-argument  : ARGUMENT
@logo-id        : ID
@logo-int       : INTEGER
