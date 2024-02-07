#lang racket

(require rackunit)
(require "CalculateScore.rkt") ; provides "calculate-score" function for getting bowling scores
(require "FileReader.rkt")     ; provides "data" which contains file contents

(define player-hash (make-hash))

; pre  -- takes a list
; post -- returns a string indicating whether the input list refers to a team or a player
(define (team-or-player? input-list)
  (if (eq? (length input-list) 1)
  "team"
  "player"))

; pre  -- takes a list pertaining to a player
; post -- splits the name and the score into separate lists
(define (player-score-split input-list)
  (define player-name (list (first input-list)
                            (second input-list)))
  (define player-score (drop input-list 2))
  (list player-name player-score))


  