#lang logo
if 2 > 1 [forward 100] [forward -100]
right 90
if 1 > 2 [forward 100]

if 2 >= 2 [print "true"]
if 2 <= 1 [print "this should not print"] [print "this should print"]
