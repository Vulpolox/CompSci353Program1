#lang racket

; function for calculating the score of a bowling game
; score-list is a list of tokens containing integers and chars "X" and "/"
(define (calculate-score score-list)
  (cond
    [(string=? (get-frame-type score-list) "spare")
     (+ (calculate-spare score-list) (calculate-score (pop-front score-list 2)))] ; calculates score from spares
    [(string=? (get-frame-type score-list) "open")
     ]
     
    )
  )

; function for determining if something is both a char and is equal to another char
(define (is-char-equal a b)
  (if (not (and (char? a) (char? b))) ; if one or both are not characters
      #f                              ; then return false
      (char=? a b)                    ; else return the result of char=? of a and b
      )
  )

; function for removing n elements from front of list; if list has less than n elements return empty list
(define (pop-front my-list n)
  (if (< (length my-list) n) ; if there are less than n elements in list
      empty                  ; return an empty list
      (drop my-list n)       ; else remove the first n elements from the list and return that
      )
  )

; function for determining frame type (spare, strike, open frame)
(define (get-frame-type score-list)
  (cond
    [(empty? score-list)
     "empty"]
    [(and (number? (first score-list))
          (is-char-equal (second score-list) #\/))
     "spare"]
    [(and (number? (first score-list))
          (number? (second score-list)))
     "open"]
    [(is-char-equal (first score-list) #\X)
     "strike"]
    [else
     "invalid frame type; make sure characters are used instead of symbols"]
  )
  )

; function for calculating spare
(define (calculate-spare score-list)
  (cond
    [(string=? (get-frame-type (pop-front score-list 2)) "strike") ; next roll is a strike
     20]
    [(string=? (get-frame-type (pop-front score-list 2)) "empty")  ; something went wrong
     -10000000]
    [else                                                          ; add 10 to the first roll of the next frame and return the sum
     (+ (first (pop-front score-list 2)) 10)] 
  )
  )

; function for calculating strike
(define (calculate-strike score-list)
  (cond
    [(and
     (string=? (get-frame-type (pop-front score-list 1)) "strike")                ; player's next two rolls are both strikes
     (string=? (get-frame-type (pop-front score-list 2)) "strike"))
     30]
    [(string=? (get-frame-type (pop-front score-list 1)) "spare")                 ; player's next two rolls result in a spare
     20]
    [(string=? (get-frame-type (pop-front score-list 1)) "open")                  ; player's next two rolls result in an open frame
     (+ (first (pop-front score-list 1))
        (second (pop-front score-list 2)))
     10]
  )
  )

; function for calculating open frame
(define (calculate-open score-list)
  (+ (first score-list) (second score-list))
  )