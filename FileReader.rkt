#lang racket

(provide data)

; pre  -- a line of text
; post -- returns a list of strings where each string is obtained from the line by splitting it by whitespace
(define (split-line input-string)
  (string-split input-string))

(define lines (file->lines "scores.txt"))     ; stores the contents of "scores.txt" as a list of the lines in it
(define data (map split-line lines))          ; splits each line in lines into a white space delimited list of strings