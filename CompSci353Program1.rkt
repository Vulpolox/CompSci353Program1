#lang racket

; function for calculating the score of a bowling game
; score-list is a list of tokens containing integers and chars "X" and "/"
(define (calculate-score score-list)
  (cond
    [(string=? (get-frame-type score-list) "spare")
     (+ (calculate-spare score-list) (calculate-score (drop 2 score-list)))] ; calculates score from spares


; function for determining frame type (spare, strike, open frame)
(define (get-frame-type score-list)
  (cond
    [(empty? score-list)
     "empty"]
    [(and (number? (first score-list))
          (char=? (second score-list) #\/))
     "spare"]
    [(and (number? (first score-list))
          (number? (second score-list)))
     "open"]
    [(char=? (first score-list) #\X)
     "strike"]
  )

; function for calculating spare
  (define (calculate-spare score-list)
    (cond
      [(string=? (get-frame-type (drop 2 score-list)) "strike")
       20]
      [else (+ (first (drop 2 score-list)) 10)]
      )