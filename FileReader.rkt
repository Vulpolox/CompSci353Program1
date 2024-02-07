#lang racket

(define lines (file->lines "scores.txt"))           ; stores the contents of "scores.txt" as a list of the lines in it
(define sub-strings (map split-line input-lines))   ; splits each line in lines into a white space delimited list of strings

sub-strings
