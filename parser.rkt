#lang brag

logo-program    : [logo-line] ((/NEWLINE)* [logo-line])*
@logo-line      : logo-statement
@logo-statement : logo-forward | logo-back | logo-right | logo-left
                | logo-repeat | logo-to | logo-if | logo-stop
                | logo-output | logo-pen-up | logo-pen-down | logo-clear
                | logo-home | logo-print | logo-random | logo-comment
                | logo-set-position | logo-set-pen-color | logo-set-pen-style
                | logo-max-width | logo-max-height
                | logo-command
logo-forward    : /("forward"|"fd") logo-expr
logo-back       : /("back"|"bk")    logo-expr
logo-right      : /("right"|"rt")   logo-expr
logo-left       : /("left"|"lt")    logo-expr
logo-repeat     : /"repeat"  logo-expr /"[" @logo-program /"]"
logo-to         : /"to" logo-id ([logo-argument])* logo-program /"end"
logo-if         : /"if" logo-not /"[" logo-program /"]" [/"[" logo-program /"]"]
logo-stop       : /"stop"
logo-output     : /"output" logo-expr
logo-pen-up     : /("penup"|"pu")
logo-pen-down   : /("pendown"|"pd")
logo-clear      : /("clear"|"clearscreen"|"cs")
logo-home       : /"home"
logo-print      : /"print" logo-expr
logo-random     : /"random" logo-expr
logo-max-width  : /"maxwidth"
logo-max-height : /"maxheight"
logo-set-position  : /("setposition"|"setpos"|"setxy") logo-expr logo-expr
logo-set-pen-color : /("setpencolor"|"setpc") ((/"[" logo-expr logo-expr logo-expr /"]")
                                               | logo-string)
logo-set-pen-style : /("setpenstyle"|"setps") logo-expr
logo-command    : logo-id ([logo-expr])*
logo-not        : ["not"] logo-cond
logo-cond       : logo-expr ("="|"<"|">"|"!="|"<="|">=") logo-expr
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
logo-comment    : COMMENT
