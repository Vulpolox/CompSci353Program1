#lang racket

(require rackunit)
(require "CalculateScore.rkt") ; provides "calculate-score" function for getting bowling scores
(require "FileReader.rkt")     ; provides "data" which contains file contents

(define team->player-hash (make-immutable-hash))
(define player->scores-has (make-hash))

; pre  -- takes a list
; post -- returns a string indicating whether the input list refers to a team or a player
(define (team-or-player? input-list)
  (if (eq? (length input-list) 1)
  "team"
  "player"))

; pre  -- takes a list pertaining to a player
; post -- returns a list containing the player name as a single string
;         and a nested list of single strings representing said player's bowling game
(define (player-score-split input-list)
  (define player-name (string-join (take input-list 2) " "))
  (define player-score (drop input-list 2))
  (list player-name player-score))

; pre  -- takes a list of single character strings representing a bowling game
; post -- converts each string in the list to a integer if numerical or a character
;         otherwise and returns the updated list
(define (type-cast-game input-list)
  (define (str->char str)                            ; helper function for converting string to char
    (string-ref str 0))
  (define (char->int-if-num chr)                     ; helper function for converting char to int if numeric
    (if (char-numeric? chr)
        (- (char->integer chr) 48)
        chr))
  (define pure-char-list (map str->char input-list)) ; intermediary list that has strings converted to chars
  (map char->int-if-num pure-char-list))

; pre  -- takes input data
; post -- returns a list of lists where each sublist contains a player,
;         their team, and the score from their game
(define (get-clean-data data [current-team "none"] [superlist '()])
  (cond
    [(empty? data)
     superlist]
    [(eq? (team-or-player? (first data))
          "team")
     (get-clean-data (cdr data) (first data) superlist)]
    [(eq? (team-or-player? (first data))
          "player")
     (define player-name (first (player-score-split (first data))))
     (define player-score (calculate-score (type-cast-game (second (player-score-split (first data))))))
     (define updated-list (cons (list (first current-team) player-name player-score) superlist))
     (get-clean-data (cdr data) current-team updated-list)]
    )
  )

(define clean-data (get-clean-data data))
clean-data

;(define test (second data))
;(team-or-player? test)
;(player-score-split test)
;(map team-or-player? data)